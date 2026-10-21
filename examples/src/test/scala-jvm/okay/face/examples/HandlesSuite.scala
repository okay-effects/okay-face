package okay.face.examples

import okay.effects.json.Json
import okay.effects.json.Json.{Str, num, obj}
import okay.effects.machine.Machine._
import okay.face.examples.Noughts._
import okay.face.html.HtmlFace
import okay.face.mcp.McpFace
import okay.face.rest.RestFace
import okay.face.{Chosen}
import okay.face.test.Plays
import okay.http.{Http, Https, Request, Response}
import okay.mcp.{Link, Links, Rpc}

/** EVERY FACE A HANDLER of the one effect: the game handled whole by each, nothing said but the program — each face's own
 * shape of the same first board, and what it learns from what the program shows: a title, the rules */
class HandlesSuite extends munit.FunSuite {
  type H[X] = Row[*[Pure] + *[Http], X]
  type L[X] = Row[*[Pure] + *[Link], X]
  val https = new Https()
  val links = new Links()

  test("REST: a session opened, the board as JSON, the one action ref, no back, the marks served") {
    val rest = RestFace()   // bound first: Scala 3 widens a word on an unbound face to Any
    val service = rest.handle(Noughts.game, vocabulary = marks.vocabulary)
    val rs = run(https.scripted(List(Request("POST", "/sessions"), Request.get("/sessions/marks")))(https.serve[H](service)))._1
    assertEquals(rs.map(_.status), List(201, 200))
    assert(rs.head.body.contains("\"rel\":\"ref\"") && rs.head.body.contains("\"back\":false") && rs.head.body.contains("You are X"), rs.head.body)
    assert(rs(1).body.contains("\"name\":\"cell\""), rs(1).body)
  }
  test("HTML: the base opens a session, the page is titled by the heading shown, the cells are links, no back, the look at /okay.css") {
    val html = HtmlFace()
    val service = html.handle(Noughts.game, look = Some(light))
    val rs = run(https.scripted(List(Request.get("/s"), Request.get("/s/s1"), Request.get("/okay.css")))(https.serve[H](service)))._1
    assertEquals(rs.map(_.status), List(303, 200, 200))
    assert(rs(1).body.contains("<title>Noughts and crosses</title>") && rs(1).body.contains("""href="/s/s1/ref/5"""") && !rs(1).body.contains("okay-back"), rs(1).body)
    assert(rs(2).body.contains(".okay-mark-cell{"), rs(2).body)
  }
  test("MCP: named by the heading, instructed by the rules shown and the marks; the tools are the move, look and back; a call plays, back has nowhere to go") {
    val mcp = McpFace()
    val served = mcp.handle(Noughts.game, vocabulary = marks.vocabulary)
    val init = Rpc.encode(Rpc.Request(num(1), "initialize", obj("protocolVersion" -> Str("2025-06-18"), "capabilities" -> Json.Obj.empty, "clientInfo" -> obj("name" -> Str("t")))))
    val lines = List(init, Rpc.encode(Rpc.Request(num(2), "tools/list", Json.Obj.empty)),
      Rpc.encode(Rpc.Request(num(3), "tools/call", obj("name" -> Str("ref"), "arguments" -> obj("key" -> Str("5"))))),
      Rpc.encode(Rpc.Request(num(4), "tools/call", obj("name" -> Str("back")))))
    val (sent, _) = run(links.scripted(lines)(served))
    assert(sent.head.contains("\"name\":\"Noughts and crosses\"") && sent.head.contains("You are X") && sent.head.contains("Marks on views"), sent.head)
    assert(sent(1).contains("\"name\":\"ref\"") && sent(1).contains("\"name\":\"back\""), sent(1))
    assert(sent(2).contains("X"), sent(2))
    assert(sent.exists(_.contains("nothing to go back to")), "a move is not kept in history: back has nowhere to go")
  }
  test("the test face: played by a script, the views as the reference") {
    val plays = Plays()
    import plays._
    plays.handle(Noughts.game)(Script.of(Chosen("ref", obj("key" -> Str("1"))), Chosen("ref", obj("key" -> Str("4"))), Chosen("ref", obj("key" -> Str("7"))))) match {
      case Played.Stuck(views, at, None) => assertEquals(views.size, 5); assertEquals(at.actions.map(_.name), List("again"))
      case other => fail(s"$other")
    }
  }
}
