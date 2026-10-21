package okay.face.examples

import okay.effects.machine.Machine._
import okay.face.swing.SwingFace

/** NOUGHTS AND CROSSES IN A SWING WINDOW: `sbt "examples/runMain okay.face.examples.NoughtsSwing"`. The implementation is
 * said once, by the import; the game is a scenario, its title, look and rules in it; the face handles it whole */
object NoughtsSwing {
  def main(args: Array[String]): Unit =
    run(SwingFace.handle(new Noughts()))
}
