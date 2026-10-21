package okay.faces.examples

import okay.effects.machine.Machine.self
import okay.faces._
import okay.faces.term.Terminal

/** NOUGHTS AND CROSSES AT THE TERMINAL: `sbt "examples/runMain okay.faces.examples.NoughtsTerminal"` — the real console
 * handles the program; the same program the test face plays by script and the MCP face serves to an agent */
object NoughtsTerminal {
  def main(args: Array[String]): Unit = { run(Terminal.console(Noughts.games)); println("bye") }
}
