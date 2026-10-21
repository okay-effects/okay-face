package okay.face.examples

import okay.effects.machine.Machine._
import okay.face.term.Terminal

/** NOUGHTS AND CROSSES AT THE TERMINAL: `sbt "examples/runMain okay.face.examples.NoughtsTerminal"` — the same program the
 * test face plays by script and the MCP face serves to an agent; here the real console answers it */
object NoughtsTerminal {
  def main(args: Array[String]): Unit = { run(Terminal.console(new Noughts())); println("bye") }
}
