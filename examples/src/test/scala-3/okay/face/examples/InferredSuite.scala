package okay.face.examples

import okay.effects.machine.Machine
import okay.effects.machine.Machine.*
import okay.effects.machine.implicits.*
import okay.face.Face
import okay.face.swing.SwingFace

/** Scala 3 infers the implementation from the implicit one: `new Noughts()`, `new SwingFace()` — the `Singleton` bound
 * keeps `Machine.type` exact, so the game's programs are the face's own `!` */
class InferredSuite extends munit.FunSuite:
  test("the game and the face over the implicit implementation, no type written; they agree on `!`"):
    val game = new Noughts()
    val face = new SwingFace()
    type F[X] = (Pure + Face)[X]   // Scala 3 alone: the row is the lambda; 3.3 wants the alias applied, 3.4+ takes `type F = Pure + Face`
    val panel = face.panel(face.faces.steps[game.Outcome, F, Pure](game.games[F]))
    assert(game.faces.fx eq Machine)
    assert(panel.getComponentCount > 0)
