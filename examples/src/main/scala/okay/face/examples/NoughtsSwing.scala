package okay.face.examples

import okay.effects.{Sum, Void}
import okay.effects.machine.Machine
import okay.face.{Face, Faces}
import okay.face.swing.SwingFace

/** NOUGHTS AND CROSSES IN A SWING WINDOW: `sbt "examples/runMain okay.face.examples.NoughtsSwing"` */
object NoughtsSwing {
  import okay.effects.machine.implicits._
  implicit val faces: Faces[Machine.type] = new Faces()
  val game = new Noughts[Machine.type]()
  val face = new SwingFace[Machine.type]()
  type F[X] = Sum[Void, Face, X]
  def main(args: Array[String]): Unit =
    face.window("Noughts and crosses", faces.steps[game.Outcome, F, Void](game.games[F]),
      (_: game.Outcome) => (), look = Some(game.look), back = false)
}
