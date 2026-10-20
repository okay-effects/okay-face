package okay.face.examples

import okay.effects.{Sum, Void}
import okay.effects.machine.Machine
import okay.effects.world.Console
import okay.face.{Face, Faces}
import okay.face.term.Terminal

/** NOUGHTS AND CROSSES AT THE TERMINAL: `sbt "examples/runMain okay.face.examples.NoughtsTerminal"` — the same program
 * the test face plays by script and the MCP face will serve to an agent */
object NoughtsTerminal {
  val faces: Faces[Machine.type] = new Faces[Machine.type]()(Machine)
  val term: Terminal[Machine.type] = new Terminal[Machine.type](faces)
  val game = new Noughts[Machine.type](faces)
  type C[X] = Sum[Void, Console, X]
  type CF[X] = Sum[C, Face, X]

  def main(args: Array[String]): Unit = {
    val played = Machine.run(term.io.realConsole(term.terminal[game.Outcome, C](faces.steps[game.Outcome, CF, C](game.game[CF](game.Board.empty)))))
    println(played.fold("bye")(o => s"the game ended: $o"))
  }
}
