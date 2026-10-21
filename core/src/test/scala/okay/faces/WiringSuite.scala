package okay.faces

import okay.effects.json.Json
import okay.effects.shape.{Shape, Day}
import okay.effects.shape.Shape._

/** okay-faces stands on okay-simple's `shape` and `json`: the wiring, nothing more, until the faces' own code lands */
class WiringSuite extends munit.FunSuite {
  final case class Who(name: String, born: Day)
  val who: Shape[Who] = record(field("name", text), field("born", date))(Who(_, _))(w => (w.name, w.born))
  test("a shape from okay-simple decodes a json from okay-simple") {
    assertEquals(who.decode(Json.obj("name" -> Json.Str("Ann"), "born" -> Json.Str("1985-03-09"))), Right(Who("Ann", Day(1985, 3, 9))))
    assertEquals(okay.effects.json.parse(Json.print(who.encode(Who("Bo", Day(2000, 2, 29))))).map(who.decode), Right(Right(Who("Bo", Day(2000, 2, 29)))))
  }
}
