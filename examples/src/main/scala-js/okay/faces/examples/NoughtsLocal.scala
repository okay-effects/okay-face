package okay.faces.examples

import okay.effects.machine.Machine.self
import okay.faces.laminar.LaminarFace

/** NOUGHTS AND CROSSES IN THE BROWSER: the whole game in the page, the Laminar face handling the program — the script
 * of `NoughtsWebLocal`'s page, linked by `sbt -Dplatforms=js examplesJS/fullLinkJS` */
object NoughtsLocal {
  def main(args: Array[String]): Unit = LaminarFace.mount(Noughts.games, look = Some(Noughts.look))
}
