package okay.face.rest

import okay.effects.{Sum, Void}
import okay.effects.json.Json
import okay.effects.json.Json.{Str, num, obj}
import okay.effects.machine.Machine
import okay.effects.shape.Shape
import okay.effects.shape.Shape.{field, int, record, text}
import okay.face.{Face, Faces, View}
import okay.http.{Http, Https, Request}

class RestFaceSuite extends munit.FunSuite {
  val faces: Faces[Machine.type] = new Faces[Machine.type]()(Machine)
  val face = RestFace(faces)
  val https: Https[Machine.type] = new Https[Machine.type]()(Machine)
  import Machine._
  import faces._
  type H[X] = Sum[Void, Http, X]
  type HF[X] = Sum[H, Face, X]

  final case class Who(name: String, age: Int)
  val who: Shape[Who] = record(field("name", text).labelled("Name"), field("age", int.check("of age")(_ >= 18)))(Who(_, _))(w => (w.name, w.age))
  def greeting: String ! HF =
    fill[Who, HF](View.heading("Who are you?"), who).flatMap { w =>
      confirm[HF](View.text(s"Greet ${w.name}, ${w.age}?")).flatMap { yes =>
        if (yes) show[HF](View.text(s"Hello, ${w.name}!")).map(_ => s"greeted ${w.name}") else pure[String, HF]("left")
      }
    }
  def talk(requests: Request*): List[okay.http.Response] = {
    val sessions = face.sessions[String, H](() => faces.dialogue[String, H](steps[String, HF, H](greeting)))
    Machine.run(https.scripted(requests.toList)(https.serve[H](sessions.service)))._1
  }

  test("a session opened, its resource with links and schemas; an action taken; a bad one a 400 with the step unchanged; back") {
    val rs = talk(Request("POST", "/sessions"), Request.get("/sessions/s1"),
      Request.json("POST", "/sessions/s1/act/submit", obj("name" -> Str("Ann"), "age" -> num(3))),
      Request.json("POST", "/sessions/s1/act/submit", obj("name" -> Str("Ann"), "age" -> num(41))),
      Request("POST", "/sessions/s1/back"), Request("POST", "/sessions/s1/act/yes"), Request.get("/sessions/nope"))
    assertEquals(rs.map(_.status), List(201, 200, 400, 200, 200, 400, 404))   // after back, `yes` is not an action of the form step
    assertEquals(rs.head.headers("Location"), "/sessions/s1")
    assert(rs.head.body.contains("\"rel\":\"submit\"") && rs.head.body.contains("\"schema\":{\"type\":\"object\""), rs.head.body)
    assert(rs(2).body.contains("age: of age") || rs(2).body.contains("\"message\":\"of age\""), rs(2).body)
    assert(rs(3).body.contains("Greet Ann, 41?") && rs(3).body.contains("\"rel\":\"yes\"") && rs(3).body.contains("\"rel\":\"back\""), rs(3).body)
    assert(rs(4).body.contains("Who are you?"))
    assertEquals(rs(5).headers("Content-Type"), "application/problem+json")
  }
  test("to the end: done, no actions but the links to self") {
    val rs = talk(Request("POST", "/sessions"), Request.json("POST", "/sessions/s1/act/submit", obj("name" -> Str("Bo"), "age" -> num(30))), Request("POST", "/sessions/s1/act/yes"))
    assert(rs(2).body.contains("Hello, Bo!") && rs(2).body.contains("\"done\":true"), rs(2).body)
    assert(!rs(2).body.contains("\"rel\":\"yes\""))
  }
  test("an event while the step waits: the resource says it listens; fired, the next request shows the step moved on") {
    val bus = okay.face.Meanwhile.manual[Int]
    def program: String ! HF = offer[Int, HF](View.text("waiting"), List(okay.face.Action.plain[Int]("skip", "Skip")(0)), bus.meanwhile).flatMap(n => show[HF](View.text(s"got $n")).map(_ => s"$n"))
    val sessions = face.sessions[String, H](() => faces.dialogue[String, H](steps[String, HF, H](program)))
    val opened = Machine.run(https.scripted(List(Request.post("/sessions", "", "application/json")))(https.serve[H](sessions.service)))._1.head
    assert(opened.body.contains("\"listening\":true"), opened.body)
    bus.fire(5)
    val after = Machine.run(https.scripted(List(Request.get("/sessions/s1")))(https.serve[H](sessions.service)))._1.head
    assert(after.body.contains("got 5") && after.body.contains("\"done\":true") && after.body.contains("\"listening\":false"), after.body)
  }
}
