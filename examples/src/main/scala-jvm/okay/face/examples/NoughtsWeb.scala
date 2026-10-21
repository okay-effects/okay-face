package okay.face.examples

import okay.effects.machine.Machine._
import okay.face.html.HtmlFace
import okay.face.rest.RestFace
import okay.http.{Http, JdkHttp, Request, Response}

/** NOUGHTS AND CROSSES SERVED: `sbt "examples/runMain okay.face.examples.NoughtsWeb [port] [host]"` — the REST face at
 * `/sessions` and the HTML face at `/s` handle the game, each whole; `/` is the thin page over the REST face, its script
 * `examplesRemoteJS`; `/local` the game in the browser, its script `examplesJS` — `sbt -Dplatforms=js examplesJS/fullLinkJS examplesRemoteJS/fullLinkJS` */
object NoughtsWeb {
  val game = new Noughts()
  val rest = RestFace.handle(game)
  val html = HtmlFace.handle(game)
  type H[X] = Row[*[Pure] + *[Http], X]
  val service: Request => Response ! H = {
    case Request("GET", "/", _, _, _) => pure(Response.html(Shell.page(game.title, "/remote.js")))
    case Request("GET", "/local", _, _, _) => pure(Response.html(Shell.page(game.title, "/local.js")))
    case Request("GET", "/remote.js", _, _, _) => pure(Shell.script("examples/remote/.js", "okay-faces-examples-remote", "noughts.remote.js").fold(e => Response.text(e, 503), js => Response.ok(js, "application/javascript; charset=utf-8")))
    case Request("GET", "/local.js", _, _, _) => pure(Shell.script("examples/.js", "okay-faces-examples", "noughts.local.js").fold(e => Response.text(e, 503), js => Response.ok(js, "application/javascript; charset=utf-8")))
    case req if req.path.startsWith("/sessions") => rest(req)
    case req => html(req)
  }
  val http = new JdkHttp()
  def main(args: Array[String]): Unit = {
    val port = args.headOption.filter(_.forall(_.isDigit)).map(_.toInt).getOrElse(8090)
    val host = args.drop(1).headOption.getOrElse("127.0.0.1")
    run(http.serve(port, host)(service))
  }
}

/** the page shell for the browser cuts: a root, the look, the page's script — one script per cut, each its own Scala.js */
object Shell {
  def page(title: String, script: String): String =
    s"""<!DOCTYPE html>
       |<html lang="en"><head><meta charset="utf-8"/><meta name="viewport" content="width=device-width, initial-scale=1"/>
       |<title>$title</title><link rel="stylesheet" href="/okay.css"/></head>
       |<body><div id="app"></div><script src="$script"></script></body></html>""".stripMargin
  /** a linked script: under `dir/target`, the project's `name`, full or fast; or the file the property names */
  def script(dir: String, name: String, property: String): Either[String, String] = {
    val candidates = sys.props.get(property).toList ++ List(s"$dir/target/scala-3.3.8/$name-opt/main.js", s"$dir/target/scala-3.3.8/$name-fastopt/main.js")
    candidates.map(new java.io.File(_)).find(_.isFile) match {
      case Some(f) => Right(new String(java.nio.file.Files.readAllBytes(f.toPath), "UTF-8"))
      case None => Left(s"no page script: `sbt -Dplatforms=js examplesJS/fullLinkJS examplesRemoteJS/fullLinkJS` makes them, or -D$property=path names one (looked at ${candidates.mkString(", ")})")
    }
  }
}
