package okay.face.html

import okay.effects.{Sum, Void}
import okay.effects.json.Json
import okay.effects.machine.Machine
import okay.effects.shape.{Schema, Shape}
import okay.effects.shape.Shape.{field, int, record, text}
import okay.face.{Face, Faces, View}
import okay.http.{Http, Https, Query, Request}

class HtmlFaceSuite extends munit.FunSuite {
  val face = new HtmlFace[Machine.type](new Faces[Machine.type]()(Machine))
  /** THE faces: the face's own, so the dialogue's path is one */
  val faces: face.faces.type = face.faces
  val https: Https[Machine.type] = new Https[Machine.type]()(Machine)
  import Machine._
  import faces._
  type H[X] = Sum[Void, Http, X]
  type HF[X] = Sum[H, Face, X]

  final case class Who(name: String, age: Int)
  val who: Shape[Who] = record(field("name", text).labelled("Name"), field("age", int.check("of age")(_ >= 18)))(Who(_, _))(w => (w.name, w.age))
  def greeting: String ! HF =
    fill[Who, HF](View.heading("Who are you?"), who).flatMap { w =>
      confirm[HF](View.text(s"Greet <${w.name}>, ${w.age}?")).flatMap { yes =>
        if (yes) show[HF](View.text(s"Hello, ${w.name}!")).map(_ => s"greeted ${w.name}") else pure[String, HF]("left")
      }
    }
  def form(kv: (String, String)*): Request = Request.post("/s/s1/act/submit", Query.print(kv.toList.groupBy(_._1).map { case (k, vs) => k -> vs.map(_._2) }), "application/x-www-form-urlencoded")
  def talk(requests: Request*): List[okay.http.Response] = {
    val pages = face.pages[String, H](() => faces.dialogue[String, H](steps[String, HF, H](greeting)), "Greeting")
    Machine.run(https.scripted(requests.toList)(https.serve[H](pages.service)))._1
  }

  test("a page with the form from the shape; post, redirect, get; the problems shown; the button; the end; escaped") {
    val rs = talk(Request.get("/"), Request.get("/s/s1"), form("name" -> "Ann", "age" -> "3"), Request.get("/s/s1"),
      form("name" -> "Ann", "age" -> "41"), Request.get("/s/s1"), Request.post("/s/s1/act/yes", "", "application/x-www-form-urlencoded"), Request.get("/s/s1"), Request.get("/s/zz"))
    assertEquals(rs.map(_.status), List(303, 200, 303, 200, 303, 200, 303, 200, 404))
    assertEquals(rs.head.headers("Location"), "/s/s1")
    assert(rs(1).body.contains("<h1>Who are you?</h1>") && rs(1).body.contains("""<input type="number" name="age" id="f-age">""") && rs(1).body.contains("""<label for="f-name">Name</label>"""), rs(1).body)
    assert(rs(3).body.contains("<li>age: of age</li>") && rs(3).body.contains("Who are you?"), rs(3).body)
    assert(rs(5).body.contains("Greet &lt;Ann&gt;, 41?") && rs(5).body.contains("""value="yes""""), rs(5).body)
    assert(rs(5).body.contains("/s/s1/back"))
    assert(rs(7).body.contains("Hello, Ann!") && rs(7).body.contains("done: greeted Ann"), rs(7).body)
  }
  test("a form's fields to JSON along the schema: nested, optional blank, checkbox, choice, list, sum") {
    val schema = Schema.Record(Vector(
      Schema.Field("name", Schema.Text(Nil), None, None, false, None), Schema.Field("age", Schema.Optional(Schema.Integer(Nil)), None, None, false, None),
      Schema.Field("ok", Schema.Bool, None, None, false, None), Schema.Field("pet", Schema.Choice(List("dog", "cat")), None, None, false, None),
      Schema.Field("tags", Schema.Many(Schema.Text(Nil), 0, None), None, None, false, None),
      Schema.Field("home", Schema.Record(Vector(Schema.Field("city", Schema.Text(Nil), None, None, false, None))), None, None, false, None),
      Schema.Field("kind", Schema.Sum(Vector("a" -> Schema.Record(Vector(Schema.Field("x", Schema.Integer(Nil), None, None, false, None))), "b" -> Schema.Text(Nil))), None, None, false, None)))
    val form = Map("name" -> List("Ann"), "age" -> List(""), "pet" -> List("cat"), "tags" -> List("x\ny\n"), "home.city" -> List("Kyiv"), "kind.type" -> List("a"), "kind.a.x" -> List("7"))
    assertEquals(Json.print(Forms.collect(schema, form)), """{"name":"Ann","ok":false,"pet":"cat","tags":["x","y"],"home":{"city":"Kyiv"},"kind":{"type":"a","x":7}}""")
    assert(Forms.field(Nil, schema, None).contains("""<select name="kind.type""""))
  }
}
