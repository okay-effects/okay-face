package okay.faces.examples

import okay.effects.machine.Machine
import okay.effects.machine.Machine._
import okay.faces.examples.Noughts._
import okay.faces.swing.SwingFace
import okay.faces.term.Terminal

/** THE IMPLEMENTATION SAID ONCE, by the import: the faces find it and their faces, the program is written over them — on
 * both compilers, shared source, no `Machine.type` written */
class InferredSuite extends munit.FunSuite {
  test("SwingFace() and Terminal() over the implicit implementation; the program handled by each") {
    val face = SwingFace()
    val term = Terminal()
    assert(face.faces.fx eq Machine); assert(term.faces.fx eq Machine)
    val panel = face.panel(face.faces.steps[Outcome, okay.faces.Face, Pure](face.faces.of(Noughts.play(Board.empty))))
    assert(panel.getComponentCount > 0)
    val io = new okay.effects.world.Io()
    val played = term.handle(Noughts.play(Board.empty))
    val (printed, result) = run(io.console(List("1", "4", "7", "again"))(played))
    assertEquals(result, Some(Won(X))); assert(printed.exists(_.contains("X won")), printed.mkString("\n"))
  }
}
