package okay.face.examples

import okay.effects.{Sum, Void}
import okay.effects.machine.Machine
import okay.face.{Face, Faces}
import okay.face.swing.SwingFace

/** NOUGHTS AND CROSSES IN A SWING WINDOW: `sbt "examples/runMain okay.face.examples.NoughtsSwing"` */
object NoughtsSwing {
  val faces: Faces[Machine.type] = new Faces[Machine.type]()(Machine)
  val face = new SwingFace[Machine.type](faces)
  val game = new Noughts[Machine.type](faces)
  type F[X] = Sum[Void, Face, X]
  def main(args: Array[String]): Unit =
    face.window("Noughts and crosses", faces.steps[game.Outcome, F, Void](game.game[F](game.Board.empty)), (o: game.Outcome) => println(s"the game ended: $o"))
}
