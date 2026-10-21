package okay.faces.examples

import okay.effects.machine.Machine.self
import okay.faces.laminar.LaminarFace

/** THE LIST IN THE BROWSER: the whole program in the page, the Laminar face handling it — the script of
 * `TodoWebLocal`'s page, its own (`examplesTodoJS`), linked by `sbt -Dplatforms=js examplesTodoJS/fullLinkJS` */
object TodoLocal {
  def main(args: Array[String]): Unit = LaminarFace.mount(Todo.keeping, look = Some(Todo.look))
}
