package okay.face.examples

import okay.effects.machine.Machine
import okay.effects.machine.Machine._
import okay.face.Face
import okay.face.swing.SwingFace
import okay.effects.world.Console
import okay.face.term.Terminal

/** THE IMPLEMENTATION SAID ONCE, by the import of its implicit: the game, the window and the terminal find their faces
 * and the implementation's type — on both compilers, shared source, no `Machine.type` written */
class InferredSuite extends munit.FunSuite {
  test("new Noughts(), new SwingFace(), new Terminal(): over the implicit implementation, agreeing on `!`") {
    val game = new Noughts()
    val face = new SwingFace()
    val term = new Terminal()
    type F[X] = Row[*[Pure] + *[Face], X]
    assert(game.faces.fx eq Machine); assert(face.faces.fx eq Machine); assert(term.faces.fx eq Machine)
    val panel = face.panel(face.faces.steps[game.Outcome, F, Pure](game.games[F]))
    assert(panel.getComponentCount > 0)
    type C[X] = Row[*[Pure] + *[Console], X]
    type CF[X] = Row[*[Pure] + *[Console] + *[Face], X]
    val (printed, _) = run(term.io.console(List("1", "4", "7"))(term.terminal[game.Outcome, C](term.faces.steps[game.Outcome, CF, C](game.game[CF](game.Board.empty)))))
    assert(printed.exists(_.contains("X won")), printed.mkString("\n"))
  }
}
