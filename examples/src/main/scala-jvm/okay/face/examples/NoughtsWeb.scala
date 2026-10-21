package okay.face.examples

import okay.effects.machine.Machine._
import okay.face.html.{HtmlFace, Shell}
import okay.face.rest.RestFace
import okay.http.{Https, JdkHttp}

/** NOUGHTS AND CROSSES SERVED: `sbt "examples/runMain okay.face.examples.NoughtsWeb [port] [host]"` — one port, four faces:
 * the REST face at `/sessions`, the HTML face at `/s`, the thin page at `/` and the game in the browser at `/local`,
 * each handling the game whole. The pages' scripts are the examples' Scala.js, linked by
 * `sbt -Dplatforms=js examplesJS/fullLinkJS examplesRemoteJS/fullLinkJS` */
object NoughtsWeb {
  val game = new Noughts()
  val https = new Https()
  val http = new JdkHttp()
  def main(args: Array[String]): Unit = run(http.serve(port(args), host(args))(https.byPrefix(
    "/sessions" -> RestFace.handle(game),
    "/s" -> HtmlFace.handle(game),
    "/" -> Shell.handle(game, "/" -> Linked("examples/remote/.js", "okay-faces-examples-remote"), "/local" -> Linked("examples/.js", "okay-faces-examples")))))
  def port(args: Array[String]): Int = args.headOption.filter(_.forall(_.isDigit)).map(_.toInt).getOrElse(8090)
  def host(args: Array[String]): String = args.drop(1).headOption.getOrElse("127.0.0.1")
}

/** a linked Scala.js script, where sbt puts it: full first, fast otherwise */
final case class Linked(dir: String, name: String) extends (() => Either[String, String]) {
  def apply(): Either[String, String] =
    List("opt", "fastopt").map(k => new java.io.File(s"$dir/target/scala-3.3.8/$name-$k/main.js")).find(_.isFile) match {
      case Some(f) => Right(new String(java.nio.file.Files.readAllBytes(f.toPath), "UTF-8"))
      case None => Left(s"no script $name: `sbt -Dplatforms=js examplesJS/fullLinkJS examplesRemoteJS/fullLinkJS` links it")
    }
}
