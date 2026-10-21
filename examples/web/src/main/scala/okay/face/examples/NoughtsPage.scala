package okay.face.examples

import com.raquo.laminar.api.L._
import okay.effects.{Sum, Void}
import okay.effects.machine.Machine
import okay.face.{Face, Faces}
import okay.face.laminar.{LaminarFace, RemoteFace}
import okay.http.Browser
import org.scalajs.dom
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/** NOUGHTS AND CROSSES IN THE BROWSER, two cuts by the root's `data-mode`: `local`, the whole game in the page under
 * the Laminar face — the same `Noughts` program as at the terminal, compiled to JavaScript; `remote`, the game on the
 * server behind the REST face and the page okay-faces' remote face, knowing no game at all. Served by `NoughtsWeb` */
object NoughtsPage {
  def main(args: Array[String]): Unit = {
    val root = dom.document.getElementById("app")
    val element = Option(root.getAttribute("data-mode")) match {
      case Some("local") =>
        val faces: Faces[Machine.type] = new Faces[Machine.type]()(Machine)
        val face = new LaminarFace[Machine.type](faces)
        val game = new Noughts[Machine.type](faces)
        type F[X] = Sum[Void, Face, X]
        face.element[game.Outcome](faces.steps[game.Outcome, F, Void](game.games[F]), (_: game.Outcome) => (), back = false)
      case _ => RemoteFace(Browser.fetch()).element("/sessions")
    }
    renderOnDomContentLoaded(root, element)
  }
}
