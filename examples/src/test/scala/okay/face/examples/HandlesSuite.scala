package okay.face.examples

import okay.effects.json.Json
import okay.effects.json.Json.{Str, num, obj}
import okay.effects.machine.Machine._
import okay.face.html.HtmlFace
import okay.face.mcp.McpFace
import okay.face.rest.RestFace
import okay.face.test.Plays
import okay.http.{Http, Https, Request, Response}
import okay.mcp.{Link, Links, Rpc}

/** EVERY FACE A HANDLER: the game handled whole by each, nothing said but the game — the same scenario, the same
 * first board, each face's own shape of it */
class HandlesSuite extends munit.FunSuite {
  val game = new Noughts()
  type H[X] = Row[*[Pure] + *[Http], X]
  type L[X] = Row[*[Pure] + *[Link], X]
  val https = new Https()
  val links = new Links()

  test("REST: a session opened, the board as JSON, the one action ref, no back, the marks served") {
    val service: Request => Response ! H = RestFace.handle(game)
    val rs = run(https.scripted(List(Request("POST", "/sessions"), Request.get("/sessions/marks")))(https.serve[H](service)))._1
    assertEquals(rs.map(_.status), List(201, 200))
    assert(rs.head.body.contains("\"rel\":\"ref\"") && rs.head.body.contains("\"back\":false") && !rs.head.body.contains("\"rel\":\"restart\""), rs.head.body)
    assert(rs(1).body.contains("\"name\":\"cell\""), rs(1).body)
  }
  test("HTML: / opens a session, the page shows the board with the cells as links, no back, the look at /okay.css") {
    val service: Request => Response ! H = HtmlFace.handle(game)
    val rs = run(https.scripted(List(Request.get("/s"), Request.get("/s/s1"), Request.get("/okay.css")))(https.serve[H](service)))._1
    assertEquals(rs.map(_.status), List(303, 200, 200))
    assert(rs(1).body.contains("<title>Noughts and crosses</title>") && rs(1).body.contains("""href="/s/s1/ref/5"""") && !rs(1).body.contains("okay-back"), rs(1).body)
    assert(rs(2).body.contains(".okay-mark-cell{"), rs(2).body)
  }
  test("MCP: the tools are the move and look, no back; a call plays; the instructions carry the marks") {
    val served: Unit ! L = McpFace.handle(game)
    val init = Rpc.encode(Rpc.Request(num(1), "initialize", obj("protocolVersion" -> Str("2025-06-18"), "capabilities" -> Json.Obj.empty, "clientInfo" -> obj("name" -> Str("t")))))
    val lines = List(init, Rpc.encode(Rpc.Request(num(2), "tools/list", Json.Obj.empty)),
      Rpc.encode(Rpc.Request(num(3), "tools/call", obj("name" -> Str("ref"), "arguments" -> obj("key" -> Str("5"))))))
    val (sent, _) = run(links.scripted(lines)(served))
    assert(sent.head.contains("Noughts and crosses") && sent.head.contains("Marks on views"), sent.head)
    assert(sent(1).contains("\"name\":\"ref\"") && !sent(1).contains("\"name\":\"back\""), sent(1))
    assert(sent(2).contains("| · | X | · |") || sent(2).contains("X"), sent(2))
  }
  test("the test face: played by a script, the views as the reference") {
    val handling = Plays.handle(game)
    import handling.plays._
    handling(Script.of(okay.face.Chosen("ref", obj("key" -> Str("1"))), okay.face.Chosen("ref", obj("key" -> Str("4"))), okay.face.Chosen("ref", obj("key" -> Str("7"))))) match {
      case Played.Stuck(views, at, None) => assertEquals(views.size, 4); assertEquals(at.actions.map(_.name), List("again"))
      case other => fail(s"$other")
    }
  }
}
