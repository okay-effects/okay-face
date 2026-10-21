package okay.face.examples

import okay.effects.machine.Machine._
import okay.face.laminar.LaminarFace

/** NOUGHTS AND CROSSES IN THE BROWSER: the whole game in the page, the Laminar face handling it — the script of
 * `/local`, linked by `sbt -Dplatforms=js examplesJS/fullLinkJS` */
object NoughtsLocal {
  def main(args: Array[String]): Unit = LaminarFace.mount(new Noughts())
}
