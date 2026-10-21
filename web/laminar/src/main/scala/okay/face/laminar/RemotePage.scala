package okay.face.laminar

import okay.http.Browser
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/** THE THIN PAGE AS A SCRIPT: a Scala.js project whose main this is (`Compile / mainClass := Some("okay.face.laminar.RemotePage")`)
 * links the remote face alone — a dialogue on the server behind its REST face at `/sessions`, this page knowing no application */
object RemotePage {
  def main(args: Array[String]): Unit = RemoteFace.mount(Browser.fetch())
}
