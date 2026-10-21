package okay.face.examples

import okay.effects.machine.Machine._
import okay.face.Face
import okay.face.swing.SwingFace

/** NOUGHTS AND CROSSES IN A SWING WINDOW: `sbt "examples/runMain okay.face.examples.NoughtsSwing"`. The implementation
 * is said once, by the import of its implicit; the game and the window find their faces over it, and its type with
 * them — no `Machine.type` anywhere, on either compiler */
object NoughtsSwing {
  val game = new Noughts()
  val face = new SwingFace()
  type F[X] = Row[*[Pure] + *[Face], X]
  def main(args: Array[String]): Unit =
    face.window("Noughts and crosses", face.faces.steps[game.Outcome, F, Pure](game.games[F]),
      (_: game.Outcome) => (), look = Some(game.look), back = false)
}
