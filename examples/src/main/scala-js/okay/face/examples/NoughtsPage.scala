package okay.face.examples

import okay.effects.machine.Machine._
import okay.face.laminar.{LaminarFace, RemoteFace}
import okay.http.Browser
import org.scalajs.dom
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/** NOUGHTS AND CROSSES IN THE BROWSER, two cuts by the root's `data-mode`: `local`, the whole game in the page, the
 * Laminar face handling it; `remote`, the game on the server behind the REST face, the thin page knowing no game.
 * Served by `NoughtsWeb` */
object NoughtsPage {
  def main(args: Array[String]): Unit =
    Option(dom.document.getElementById("app")).map(_.getAttribute("data-mode")) match {
      case Some("local") => LaminarFace.mount(new Noughts())
      case _ => RemoteFace.mount(Browser.fetch())
    }
}
