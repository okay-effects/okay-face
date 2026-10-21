package okay.face.examples

import okay.effects.machine.Machine._
import okay.face.swing.SwingFace

object NoughtsSwing {
  def main(args: Array[String]): Unit =
    run(SwingFace.handle(new Noughts()))
}
