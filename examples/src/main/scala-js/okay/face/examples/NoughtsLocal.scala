package okay.face.examples

import okay.effects.machine.Machine._
import okay.face.laminar.LaminarFace

/** NOUGHTS AND CROSSES IN THE BROWSER: the whole game in the page, the Laminar face handling the program — the script of
 * `NoughtsWebLocal`'s page, linked by `sbt -Dplatforms=js examplesJS/fullLinkJS` */
object NoughtsLocal {
  val face = LaminarFace()   // bound first: Scala 3's compiler trips on the face made inside the call
  def main(args: Array[String]): Unit = face.mount(Noughts.games, look = Some(Noughts.light))
}
