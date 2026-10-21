package okay.face.examples

import okay.effects.machine.Machine._
import okay.face.html.HtmlFace
import okay.face.rest.RestFace
import okay.http.{Http, JdkHttp, Request, Response}

/** NOUGHTS AND CROSSES SERVED: `sbt "examples/runMain okay.face.examples.NoughtsWeb [port] [host]"` — the REST face at
 * `/sessions` and the HTML face at `/s` handle the game, each whole; `/` is the thin page over the REST face, `/local`
 * the game in the browser, the page's script from `examples/web` (`sbt -Dplatforms=js examplesWeb/fullLinkJS`) */
object NoughtsWeb {
  val game = new Noughts()
  val rest = RestFace.handle(game)
  val html = HtmlFace.handle(game)
  type H[X] = Row[*[Pure] + *[Http], X]
  val service: Request => Response ! H = {
    case Request("GET", "/", _, _, _) => pure(Response.html(Shell.page(game.title, "remote")))
    case Request("GET", "/local", _, _, _) => pure(Response.html(Shell.page(game.title, "local")))
    case Request("GET", "/app.js", _, _, _) => pure(Shell.script.fold(e => Response.text(e, 503), js => Response.ok(js, "application/javascript; charset=utf-8")))
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

/** the page shell for the browser cuts: a root with its mode, the look, the script */
object Shell {
  def page(title: String, mode: String): String =
    s"""<!DOCTYPE html>
       |<html lang="en"><head><meta charset="utf-8"/><meta name="viewport" content="width=device-width, initial-scale=1"/>
       |<title>$title</title><link rel="stylesheet" href="/okay.css"/></head>
       |<body><div id="app" data-mode="$mode"></div><script src="/app.js"></script></body></html>""".stripMargin
  def script: Either[String, String] = {
    val candidates = sys.props.get("noughts.js").toList ++ List("examples/web/target/scala-3.3.8/okay-faces-examples-web-opt/main.js", "examples/web/target/scala-3.3.8/okay-faces-examples-web-fastopt/main.js")
    candidates.map(new java.io.File(_)).find(_.isFile) match {
      case Some(f) => Right(new String(java.nio.file.Files.readAllBytes(f.toPath), "UTF-8"))
      case None => Left(s"no page script: `sbt -Dplatforms=js examplesWeb/fullLinkJS` makes it, or -Dnoughts.js=path names it (looked at ${candidates.mkString(", ")})")
    }
  }
}
