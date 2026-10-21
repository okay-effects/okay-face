package okay.face.examples

import okay.effects.machine.Machine._
import okay.face.mcp.McpFace

/** NOUGHTS AND CROSSES FOR AN AGENT: `sbt "examples/runMain okay.face.examples.NoughtsMcp"` — the game served over the
 * Model Context Protocol on standard input and output; the agent plays by the cells as tools */
object NoughtsMcp {
  def main(args: Array[String]): Unit = run(McpFace.stdio(new Noughts()))
}
