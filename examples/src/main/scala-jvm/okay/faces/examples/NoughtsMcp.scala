package okay.faces.examples

import okay.effects.machine.Machine.self
import okay.faces._
import okay.faces.mcp.McpFace

/** NOUGHTS AND CROSSES FOR AN AGENT: `sbt "examples/runMain okay.faces.examples.NoughtsMcp"` — the program served over the
 * Model Context Protocol on standard input and output; the agent plays by the cells as tools, told the marks */
object NoughtsMcp {
  def main(args: Array[String]): Unit = run(McpFace.stdio(Noughts.games, vocabulary = Noughts.marks.vocabulary))
}
