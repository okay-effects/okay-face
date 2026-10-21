package okay.face.examples

import okay.effects.machine.Machine
import okay.effects.machine.Machine._
import okay.face.{Face, Step}
import okay.face.examples.Noughts._

/** the row by its name, one spelling for both compilers: a program of a face is `A ! Row[*[Pure] + *[Face], X]`, the faces' `Only` */
class RowSuite extends munit.FunSuite {
  test("the game is a program over Pure + Face, named with + and *, applied by Row; on the machine, its first step tells the rules") {
    type F[X] = Row[*[Pure] + *[Face], X]
    val faces = okay.face.Faces.of[Machine.type]
    val p: Outcome ! F = Noughts.game.on(Machine)
    run(faces.steps[Outcome, F, Pure](p)) match {
      case Step.Told(v, _) => assertEquals(v, rules)
      case other => fail(s"$other")
    }
  }
}
