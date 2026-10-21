package okay.face

import okay.effects.Effects
import okay.effects.machine.Machine
import okay.effects.machine.Machine.self

/** the faces by default: from the implementation in scope; stateless, so any two over one implementation agree */
class DefaultFacesSuite extends munit.FunSuite {
  class Taker[F <: Effects with Singleton](implicit val faces: Faces[F])
  test("a taker of implicit faces finds them from the implicit implementation, over that implementation") {
    val a = new Taker[Machine.type]; val b = new Taker[Machine.type]
    assert(a.faces.fx eq Machine); assert(b.faces.fx eq Machine); assert(Faces.of[Machine.type].fx eq Machine)
  }
  test("steps made on one faces run on another over the same implementation") {
    import Machine._
    type F[X] = okay.effects.Sum[okay.effects.Void, Face, X]
    val a = Faces.of[Machine.type]; val b = Faces.of[Machine.type]
    val step = run(a.steps[Int, F, okay.effects.Void](b.show[F](View.text("hi")).map(_ => 7)))
    step match {
      case Step.Told(v, next) => assertEquals(v, View.text("hi")); assertEquals(run(next), Step.Done[Int, ({ type L[X] = X ! okay.effects.Void })#L](7))
      case other => fail(s"$other")
    }
  }
  test("an explicit Faces wins when it is the one in scope") {
    val own: Faces[Machine.type] = new Faces[Machine.type]()(Machine)
    implicit val mine: Faces[Machine.type] = own
    assert(new Taker[Machine.type].faces eq own)
  }
}
