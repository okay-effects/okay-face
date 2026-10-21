package okay.face.rest

import okay.effects.machine.Machine._
import okay.http.{Http, Https, Request, Response}

/** the page of a script: each page at its path, its script beside it, the look; the script read when asked */
class ShellSuite extends munit.FunSuite {
  val https = new Https()
  type H[X] = Row[*[Pure] + *[Http], X]
  test("pages, scripts, the stylesheet; a script that is not there is a 503 with the reason") {
    val service: Request => Response ! H = Shell().handle("Hello & co", None, "/" -> (() => Right("console.log(1)")), "/local" -> (() => Left("not linked")))
    val rs = run(https.scripted(List(Request.get("/"), Request.get("/index.js"), Request.get("/local"), Request.get("/local.js"), Request.get("/okay.css"), Request.get("/x")))(https.serve[H](service)))._1
    assertEquals(rs.map(_.status), List(200, 200, 200, 503, 200, 404))
    assert(rs.head.body.contains("<title>Hello &amp; co</title>") && rs.head.body.contains("""<script src="/index.js">""") && rs.head.body.contains("""<div id="app">"""), rs.head.body)
    assertEquals(rs(1).body, "console.log(1)"); assertEquals(rs(3).body, "not linked")
    assert(rs(2).body.contains("""<script src="/local.js">"""))
  }
}
