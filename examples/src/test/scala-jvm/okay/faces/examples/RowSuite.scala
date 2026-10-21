package okay.faces.examples

import okay.effects.machine.Machine
import okay.effects.machine.Machine._
import okay.faces.{Face, Step}
import okay.faces.examples.Noughts._

/** the row by its name, one spelling for both compilers: a program of a face is `A ! Face`, one effect its own row; it goes
 * into a wider one, `A ! Row[*[Pure] + *[Face], X]`, by `into` */
class RowSuite extends munit.FunSuite {
  test("the game is a program over Face; into Pure + Face, named with + and *, applied by Row; on the machine, its first step tells the rules") {
    type F[X] = Row[*[Pure] + *[Face], X]
    val faces = okay.faces.Faces.of[Machine.type]
    val p: Outcome ! F = Noughts.game.on(Machine).into[F]
    run(faces.steps[Outcome, F, Pure](p)) match {
      case Step.Told(v, _) => assertEquals(v, rules)
      case other => fail(s"$other")
    }
  }
}
