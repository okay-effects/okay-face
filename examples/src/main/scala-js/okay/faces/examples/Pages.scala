package okay.faces.examples

import okay.effects.machine.Machine.self
import okay.faces.laminar.LaminarFace
import org.scalajs.dom

/** THE PAGES IN THE BROWSER: one script for the examples, the program chosen by the page's title — the shell page
 * `TodoWebLocal` serves is titled "To do", `NoughtsWebLocal`'s "Noughts and crosses"; the Laminar face handles
 * whichever, whole, in the page. Linked by `sbt -Dplatforms=js examplesJS/fullLinkJS` */
object Pages {
  def main(args: Array[String]): Unit = dom.document.title match {
    case "To do" => LaminarFace.mount(Todo.keeping, look = Some(Todo.look))
    case _ => LaminarFace.mount(Noughts.games, look = Some(Noughts.look))
  }
}
