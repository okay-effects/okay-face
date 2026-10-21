package okay.faces

import okay.effects.machine.Machine
import okay.effects.machine.Machine.self

/** faces over an implementation: stateless, so any two over one implementation agree */
class FacesSuite extends munit.FunSuite {
  test("steps made on one faces run on another over the same implementation") {
    import Machine._
    type F[X] = okay.effects.Sum[okay.effects.Void, Face, X]
    val a = Faces.of[Machine.type]; val b = Faces()
    assert(a.fx eq Machine); assert(b.fx eq Machine)
    val step = run(a.steps[Int, F, okay.effects.Void](b.show[F](View.text("hi")).map(_ => 7)))
    step match {
      case Step.Told(v, next) =>
        assertEquals(v, View.text("hi"));
        assertEquals(run(next), Step.Done[Int, ({ type L[X] = X ! okay.effects.Void })#L](7))
      case other => fail(s"$other")
    }
  }
}
