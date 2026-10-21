package okay.face

import okay.effects.Effects
import okay.effects.machine.Machine
import okay.effects.machine.implicits._

/** the faces by default: from the implementation in scope, one instance per implementation */
class DefaultFacesSuite extends munit.FunSuite {
  class Taker[F <: Effects](implicit val faces: Faces[F])
  test("a taker of implicit faces finds them from the implicit implementation; two takers share one instance") {
    val a = new Taker[Machine.type]; val b = new Taker[Machine.type]
    assert(a.faces eq b.faces); assert(a.faces eq Faces.of[Machine.type]); assert(a.faces.fx eq Machine)
  }
  test("an explicit Faces is another instance, and wins when it is the one in scope") {
    val own: Faces[Machine.type] = new Faces[Machine.type]()(Machine)
    assert(own ne Faces.of[Machine.type])
    implicit val mine: Faces[Machine.type] = own
    assert(new Taker[Machine.type].faces eq own)
  }
}
