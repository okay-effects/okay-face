package okay.faces.examples

import okay.effects.machine.Machine.self
import okay.faces._
import okay.faces.swing.SwingFace

/** NOUGHTS AND CROSSES IN A SWING WINDOW: `sbt "examples/runMain okay.faces.examples.NoughtsSwing"` — the window handles
 * the program; the implementation is said once, by the import */
object NoughtsSwing {
  def main(args: Array[String]): Unit = run(SwingFace.handle(Noughts.games, look = Some(Noughts.light)))
}
