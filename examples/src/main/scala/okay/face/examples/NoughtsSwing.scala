package okay.face.examples

import okay.effects.machine.Machine
import okay.effects.machine.Machine._
import okay.effects.machine.implicits._
import okay.face.Face
import okay.face.swing.SwingFace

/** NOUGHTS AND CROSSES IN A SWING WINDOW: `sbt "examples/runMain okay.face.examples.NoughtsSwing"`. The machine is the
 * implicit implementation, and the faces over it come by default: the game and the window find the same ones */
object NoughtsSwing {
  val game = new Noughts[Machine.type]()
  val face = new SwingFace[Machine.type]()
  type F[X] = Row[*[Pure] + *[Face], X]   // the row by its name, the one spelling both compilers read
  def main(args: Array[String]): Unit =
    face.window("Noughts and crosses", face.faces.steps[game.Outcome, F, Pure](game.games[F]),
      (_: game.Outcome) => (), look = Some(game.look), back = false)
}
