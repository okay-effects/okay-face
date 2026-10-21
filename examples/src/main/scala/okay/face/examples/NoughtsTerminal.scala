package okay.face.examples

import okay.effects.machine.Machine._
import okay.effects.world.Io
import okay.face.term.Terminal

/** NOUGHTS AND CROSSES AT THE TERMINAL: `sbt "examples/runMain okay.face.examples.NoughtsTerminal"` — the same program the
 * test face plays by script and the MCP face serves to an agent; here the real console answers it */
object NoughtsTerminal {
  val game = new Noughts()
  def main(args: Array[String]): Unit = { run(new Io().realConsole(Terminal.handle(game))); println("bye") }
}
