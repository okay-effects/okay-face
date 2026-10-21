package okay.face.examples

import okay.effects.machine.Machine.self
import okay.face._
import okay.face.swing.SwingFace

/** NOUGHTS AND CROSSES IN A SWING WINDOW: `sbt "examples/runMain okay.face.examples.NoughtsSwing"` — the window handles
 * the program; the implementation is said once, by the import */
object NoughtsSwing {
  def main(args: Array[String]): Unit = run(SwingFace.handle(Noughts.games, look = Some(Noughts.light)))
}
