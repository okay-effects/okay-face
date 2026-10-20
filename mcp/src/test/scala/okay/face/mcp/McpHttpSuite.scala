package okay.face.mcp

import okay.effects.{Sum, Void}
import okay.effects.json.Json
import okay.effects.json.Json.{Str, num, obj}
import okay.effects.machine.Machine
import okay.face.{Face, Faces, View}
import okay.http.{Http, Https, Request}
import okay.mcp.Rpc

/** MCP over HTTP: a session opened by initialize and named by the header, the tools listed and called, a batch, a
 * notification alone 202, a session each its own dialogue, ended by DELETE */
class McpHttpSuite extends munit.FunSuite {
  val faces: Faces[Machine.type] = new Faces[Machine.type]()(Machine)
  val face = McpFace(faces)
  val https: Https[Machine.type] = new Https[Machine.type]()(Machine)
  import Machine._
  import faces._
  type H[X] = Sum[Void, Http, X]
  type HF[X] = Sum[H, Face, X]
  def greeting: String ! HF = confirm[HF](View.text("Go?")).flatMap(yes => show[HF](View.text(if (yes) "went" else "stayed")).map(_ => if (yes) "went" else "stayed"))
  val service = face.http.service[H](() => face.server[String, H]("t", "0", steps[String, HF, H](greeting)))
  def post(body: String, session: Option[String] = None): Request =
    Request.post("/mcp", body).copy(headers = Map("content-type" -> List("application/json")) ++ session.map(s => "mcp-session-id" -> List(s)))
  def msg(m: Rpc): String = Rpc.encode(m)
  val init = msg(Rpc.Request(num(1), "initialize", obj("protocolVersion" -> Str("2025-06-18"), "capabilities" -> Json.Obj.empty, "clientInfo" -> obj("name" -> Str("t")))))

  test("initialize opens a session named by the header; list and call with it; a notification alone is 202; a batch answers an array; delete ends it") {
    val first = Machine.run(https.scripted(List(post(init)))(https.serve[H](service.service)))._1.head
    assertEquals(first.status, 200); val id = first.headers("Mcp-Session-Id"); assertEquals(id, "m1")
    assert(first.body.contains("protocolVersion"), first.body)
    val rs = Machine.run(https.scripted(List(
      post(msg(Rpc.Notify("notifications/initialized", Json.Obj.empty)), Some(id)),
      post(msg(Rpc.Request(num(2), "tools/list", Json.Obj.empty)), Some(id)),
      post("[" + msg(Rpc.Request(num(3), "tools/call", obj("name" -> Str("yes"), "arguments" -> Json.Obj.empty))) + "," + msg(Rpc.Request(num(4), "ping", Json.Obj.empty)) + "]", Some(id)),
      post(msg(Rpc.Request(num(5), "tools/list", Json.Obj.empty)), Some("nope")),
      post(msg(Rpc.Request(num(6), "tools/list", Json.Obj.empty))),
      Request("DELETE", "/mcp").copy(headers = Map("mcp-session-id" -> List(id))),
      post(msg(Rpc.Request(num(7), "tools/list", Json.Obj.empty)), Some(id))))(https.serve[H](service.service)))._1
    assertEquals(rs.map(_.status), List(202, 200, 200, 404, 400, 200, 404))
    assert(rs(1).body.contains("\"yes\"") && rs(1).body.contains("\"no\""), rs(1).body)
    assert(rs(2).body.startsWith("[") && rs(2).body.contains("went") && rs(2).body.contains("\"id\":4"), rs(2).body)
    assert(rs(4).body.contains("initialize first"), rs(4).body)
  }
  test("each session its own dialogue") {
    val a = Machine.run(https.scripted(List(post(init)))(https.serve[H](service.service)))._1.head.headers("Mcp-Session-Id")
    val b = Machine.run(https.scripted(List(post(init)))(https.serve[H](service.service)))._1.head.headers("Mcp-Session-Id")
    assertNotEquals(a, b)
    val rs = Machine.run(https.scripted(List(
      post(msg(Rpc.Request(num(2), "tools/call", obj("name" -> Str("no"), "arguments" -> Json.Obj.empty))), Some(a)),
      post(msg(Rpc.Request(num(3), "tools/call", obj("name" -> Str("look"), "arguments" -> Json.Obj.empty))), Some(b))))(https.serve[H](service.service)))._1
    assert(rs(0).body.contains("stayed") && rs(1).body.contains("Go?") && !rs(1).body.contains("stayed"), rs.map(_.body).toString)
  }
}
