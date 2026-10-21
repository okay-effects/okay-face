package okay.face.examples

import okay.effects.machine.Machine._
import okay.effects.world.Console
import okay.face.Face
import okay.face.term.Terminal

/** NOUGHTS AND CROSSES AT THE TERMINAL: `sbt "examples/runMain okay.face.examples.NoughtsTerminal"` — the same program
 * the test face plays by script and the MCP face serves to an agent. The implementation is said once, by the import
 * of its implicit; the rows are named, one spelling for both compilers */
object NoughtsTerminal {
  val game = new Noughts()
  val term = new Terminal()
  type C[X] = Row[*[Pure] + *[Console], X]
  type CF[X] = Row[*[Pure] + *[Console] + *[Face], X]

  def main(args: Array[String]): Unit = {
    run(term.io.realConsole(term.terminal[game.Outcome, C](term.faces.steps[game.Outcome, CF, C](game.games[CF]), back = false)))
    println("bye")
  }
}
