package okay.face.examples

import okay.effects.machine.Machine
import okay.effects.machine.Machine._
import okay.effects.machine.implicits._
import okay.face.Face

/** the row by its name, one spelling for both compilers: `Row[*[Pure] + *[Face], X]` — the game over it */
class RowSuite extends munit.FunSuite {
  test("the game over a row named with + and *, applied by Row") {
    val game = new Noughts[Machine.type]()
    type F[X] = Row[*[Pure] + *[Face], X]
    type M[X] = X ! Pure
    run(game.faces.steps[game.Outcome, F, Pure](game.games[F])) match {
      case w: okay.face.Step.Waiting[game.Outcome, M] @unchecked => assertEquals(w.actions.map(_.name), List("ref"))
      case other => fail(s"$other")
    }
  }
}
