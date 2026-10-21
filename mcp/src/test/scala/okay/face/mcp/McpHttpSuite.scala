package okay.face.mcp

import okay.effects.{Sum, Void}
import okay.effects.json.Json
import okay.effects.json.Json.{Str, num, obj}
import okay.effects.machine.Machine
import okay.face.{Face, Faces, View}
import okay.http.{Http, Https, Request}
import okay.mcp.{Link, Rpc, ToolCall, ToolResult, ToolSpec}

/** MCP over HTTP: a session opened by initialize and named by the header, the tools listed and called, a batch, a
 * notification alone 202, a session each its own dialogue, ended by DELETE; a stream accepted carries the server's
 * own messages, and an elicitation parks the loop until the client's answer arrives in a later POST */
class McpHttpSuite extends munit.FunSuite {
  val faces: Faces[Machine.type] = new Faces[Machine.type]()(Machine)
  val face = McpFace.over(faces)
  val https: Https[Machine.type] = new Https[Machine.type]()(Machine)
  import Machine._
  import faces._
  type H[X] = Sum[Void, Http, X]
  type HL[X] = Sum[H, Link, X]
  type HLF[X] = Sum[HL, Face, X]
  def greeting: String ! HLF = confirm[HLF](View.text("Go?")).flatMap(yes => show[HLF](View.text(if (yes) "went" else "stayed")).map(_ => if (yes) "went" else "stayed"))
  val service = face.http.service[H](_ => face.server[String, HL]("t", "0", steps[String, HLF, HL](greeting)))
  def post(body: String, session: Option[String] = None, stream: Boolean = false): Request =
    Request.post("/mcp", body).copy(headers = Map("content-type" -> List("application/json")) ++ (if (stream) Map("accept" -> List("application/json, text/event-stream")) else Map.empty) ++ session.map(s => "mcp-session-id" -> List(s)))
  def msg(m: Rpc): String = Rpc.encode(m)
  val init = msg(Rpc.Request(num(1), "initialize", obj("protocolVersion" -> Str("2025-06-18"), "capabilities" -> Json.Obj.empty, "clientInfo" -> obj("name" -> Str("t")))))
  def talk(svc: face.http.Service[H], rs: Request*): List[okay.http.Response] = Machine.run(https.scripted(rs.toList)(https.serve[H](svc.service)))._1

  test("initialize opens a session named by the header; list and call with it; a notification alone is 202; a batch answers an array; delete ends it") {
    val first = talk(service, post(init)).head
    assertEquals(first.status, 200); val id = first.headers("Mcp-Session-Id"); assertEquals(id, "m1")
    assert(first.body.contains("protocolVersion"), first.body)
    val rs = talk(service,
      post(msg(Rpc.Notify("notifications/initialized", Json.Obj.empty)), Some(id)),
      post(msg(Rpc.Request(num(2), "tools/list", Json.Obj.empty)), Some(id)),
      post("[" + msg(Rpc.Request(num(3), "tools/call", obj("name" -> Str("yes"), "arguments" -> Json.Obj.empty))) + "," + msg(Rpc.Request(num(4), "ping", Json.Obj.empty)) + "]", Some(id)),
      post(msg(Rpc.Request(num(5), "tools/list", Json.Obj.empty)), Some("nope")),
      post(msg(Rpc.Request(num(6), "tools/list", Json.Obj.empty))),
      Request("DELETE", "/mcp").copy(headers = Map("mcp-session-id" -> List(id))),
      post(msg(Rpc.Request(num(7), "tools/list", Json.Obj.empty)), Some(id)))
    assertEquals(rs.map(_.status), List(202, 200, 200, 404, 400, 200, 404))
    assert(rs(1).body.contains("\"yes\"") && rs(1).body.contains("\"no\""), rs(1).body)
    assert(rs(2).body.startsWith("[") && rs(2).body.contains("went") && rs(2).body.contains("\"id\":4"), rs(2).body)
    assert(rs(4).body.contains("initialize first"), rs(4).body)
  }
  test("each session its own dialogue") {
    val a = talk(service, post(init)).head.headers("Mcp-Session-Id")
    val b = talk(service, post(init)).head.headers("Mcp-Session-Id")
    assertNotEquals(a, b)
    val rs = talk(service,
      post(msg(Rpc.Request(num(2), "tools/call", obj("name" -> Str("no"), "arguments" -> Json.Obj.empty))), Some(a)),
      post(msg(Rpc.Request(num(3), "tools/call", obj("name" -> Str("look"), "arguments" -> Json.Obj.empty))), Some(b)))
    assert(rs(0).body.contains("stayed") && rs(1).body.contains("Go?") && !rs(1).body.contains("stayed"), rs.map(_.body).toString)
  }
  test("a stream accepted: the answers as events with a list_changed among them; an elicitation parks the loop, the client's answer in a later POST resumes it") {
    // a server whose one tool asks the user by elicitation, in the row with the Link, and answers what they said
    val asked = new face.servers.Tools[HL] {
      def list: List[ToolSpec] ! HL = pure[List[ToolSpec], HL](List(ToolSpec("ask", "asks the user", Json.obj("type" -> Json.Str("object")))))
      def call(c: ToolCall): ToolResult ! HL = face.servers.elicit[HL]("go?", Json.obj("type" -> Json.Str("object"))).map {
        case Right(content) => ToolResult.text("accepted " + Json.print(content))
        case Left(d) => ToolResult.text("declined: " + d.action)
      }
    }
    val asking = face.http.service[H](stream => face.servers.Server[HL]("asking", "0", asked, None, None, elicitation = stream))
    val opened = talk(asking, post(init, stream = true)).head
    assertEquals(opened.headers("Content-Type"), "text/event-stream"); assert(opened.body.contains("\"elicitation\""), opened.body)
    val id = opened.headers("Mcp-Session-Id")
    val asked1 = talk(asking, post(msg(Rpc.Request(num(2), "tools/call", obj("name" -> Str("ask"), "arguments" -> Json.Obj.empty))), Some(id), stream = true)).head
    assert(asked1.body.contains("elicitation/create") && asked1.body.contains("elicit-1") && !asked1.body.contains("\"id\":2"), "the question went, the answer not yet: " + asked1.body)
    val answered = talk(asking, post(msg(Rpc.Answer(Str("elicit-1"), obj("action" -> Str("accept"), "content" -> obj("x" -> num(1))))), Some(id), stream = true)).head
    assert(answered.body.contains("\"id\":2") && answered.body.contains("accepted {\\\"x\\\":1}"), answered.body)
    // the dialogue's list_changed rides the stream after a call that changed the tools
    val dialogue = talk(service, post(init, stream = true)).head
    val d = dialogue.headers("Mcp-Session-Id")
    talk(service, post(msg(Rpc.Request(num(1), "tools/list", Json.Obj.empty)), Some(d), stream = true))   // the list known, so a change is told
    val went = talk(service, post(msg(Rpc.Request(num(2), "tools/call", obj("name" -> Str("yes"), "arguments" -> Json.Obj.empty))), Some(d), stream = true)).head
    assert(went.body.contains("event: message") && went.body.contains("list_changed") && went.body.contains("went"), went.body)
    // without a stream, no elicitation is offered and a call is plain JSON
    val plain = talk(asking, post(init)).head
    assert(!plain.body.contains("\"elicitation\""), plain.body)
  }
}
