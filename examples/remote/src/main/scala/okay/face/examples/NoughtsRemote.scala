package okay.face.examples

import okay.face.laminar.RemoteFace
import okay.http.Browser
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/** THE THIN PAGE: the game on the server behind its REST face, this script knowing no game — the script of `/`,
 * linked by `sbt -Dplatforms=js examplesRemoteJS/fullLinkJS`; the same script would do for any scenario served */
object NoughtsRemote {
  def main(args: Array[String]): Unit = RemoteFace.mount(Browser.fetch())
}
