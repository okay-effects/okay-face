package okay.face.examples

import okay.effects.machine.Machine.self
import okay.face._
import okay.face.rest.{Linked, Shell}

/** THE GAME IN THE BROWSER: `sbt "examples/runMain okay.face.examples.NoughtsWebLocal [port] [host]"` — the server serves
 * the page and `NoughtsLocal`'s script, the game runs in the page; linked by `sbt -Dplatforms=js examplesJS/fullLinkJS` */
object NoughtsWebLocal {
  def main(args: Array[String]): Unit = run(Shell.serve("Noughts and crosses", Args.port(args), Args.host(args), Linked("examples/.js", "okay-faces-examples"), look = Some(Noughts.light)))
}
