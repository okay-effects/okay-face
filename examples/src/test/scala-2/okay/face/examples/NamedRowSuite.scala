package okay.face.examples

import okay.effects.machine.Machine
import okay.effects.machine.Machine._
import okay.effects.machine.implicits._
import okay.face.Face

/** Scala 2 writes the row by names: `+` of names, the row itself their `L` — the same game over it */
class NamedRowSuite extends munit.FunSuite {
  test("the game over a row written with + as a name, Pure + Face, its L the row") {
    val game = new Noughts[Machine.type]()
    type F[X] = (*[Pure] + *[Face])#L[X]
    type M[X] = X ! Pure
    run(game.faces.steps[game.Outcome, F, Pure](game.games[F])) match {
      case w: okay.face.Step.Waiting[game.Outcome, M] @unchecked => assertEquals(w.actions.map(_.name), List("ref"))
      case other => fail(s"$other")
    }
  }
}
