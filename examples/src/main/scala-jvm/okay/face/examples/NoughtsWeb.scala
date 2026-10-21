package okay.face.examples

import okay.effects.machine.Machine._
import okay.face.html.{HtmlFace, Linked, Shell, Web}
import okay.face.rest.RestFace

/** NOUGHTS AND CROSSES SERVED: `sbt "examples/runMain okay.face.examples.NoughtsWeb [port] [host]"` — one port, four faces:
 * the REST face at `/sessions`, the HTML face at `/s`, the thin page at `/` and the game in the browser at `/local`,
 * each handling the game whole. The pages' scripts are the examples' Scala.js, linked by
 * `sbt -Dplatforms=js examplesJS/fullLinkJS examplesRemoteJS/fullLinkJS` */
object NoughtsWeb {
  val game = new Noughts()
  def main(args: Array[String]): Unit = run(Web.serve(game, port(args), host(args))(
    "/sessions" -> RestFace.handle(game),
    "/s" -> HtmlFace.handle(game),
    "/" -> Shell.handle(game,
      "/" -> Linked("examples/remote/.js", "okay-faces-examples-remote"),
      "/local" -> Linked("examples/.js", "okay-faces-examples"))))
  def port(args: Array[String]): Int = args.headOption.filter(_.forall(_.isDigit)).map(_.toInt).getOrElse(8090)
  def host(args: Array[String]): String = args.drop(1).headOption.getOrElse("127.0.0.1")
}
