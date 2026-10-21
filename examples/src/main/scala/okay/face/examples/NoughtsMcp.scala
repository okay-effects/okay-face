package okay.face.examples

import okay.effects.machine.Machine._
import okay.face.mcp.McpFace
import okay.mcp.{Link, Links}

/** NOUGHTS AND CROSSES FOR AN AGENT: `sbt "examples/runMain okay.face.examples.NoughtsMcp"` — the game served over the
 * Model Context Protocol on standard input and output; the agent plays by the cells as tools */
object NoughtsMcp {
  val game = new Noughts()
  type L[X] = Row[*[Pure] + *[Link], X]
  val served: Unit ! L = McpFace.handle(game)
  val links = new Links()
  def main(args: Array[String]): Unit = run(links.stdio(served))
}
