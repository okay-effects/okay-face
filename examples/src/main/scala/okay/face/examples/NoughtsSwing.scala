package okay.face.examples

import okay.effects.Sum
import okay.effects.machine.Machine
import okay.effects.machine.Machine.Pure
import okay.effects.machine.implicits._
import okay.face.Face
import okay.face.swing.SwingFace

/** NOUGHTS AND CROSSES IN A SWING WINDOW: `sbt "examples/runMain okay.face.examples.NoughtsSwing"`. The machine is the
 * implicit implementation, and the faces over it come by default: the game and the window find the same ones */
object NoughtsSwing {
  val game = new Noughts[Machine.type]()
  val face = new SwingFace[Machine.type]()
  type F[X] = Sum[Pure, Face, X]   // Scala 3 alone would say `type F = Pure + Face`; this is the one spelling both compilers read
  def main(args: Array[String]): Unit =
    face.window("Noughts and crosses", face.faces.steps[game.Outcome, F, Pure](game.games[F]), (_: game.Outcome) => (), look = Some(game.look), back = false)
}
