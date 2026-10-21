package okay.face.examples

import okay.effects.{Sum, Void}
import okay.effects.machine.Machine
import okay.face.{Css, Face, Faces}
import okay.face.rest.RestFace
import okay.http.{Http, Https, JdkHttp, JdkServer, Request, Response}

/** NOUGHTS AND CROSSES SERVED: `sbt "examples/runMain okay.face.examples.NoughtsWeb [port] [host]"` — `/` is the game
 * on the server behind the REST face (`/sessions`) with the remote face as the page; `/local` the same game running in
 * the browser; `/okay.css` the look. The page's script is `examples/web`, linked by `sbt -Dplatforms=js examplesWeb/fullLinkJS`
 * (or `-Dnoughts.js=path`). The host `0.0.0.0` opens it to the network */
object NoughtsWeb {
  import okay.effects.machine.implicits._
  implicit val faces: Faces[Machine.type] = new Faces()
  val game = new Noughts[Machine.type]()
  type H[X] = Sum[Void, Http, X]
  type HF[X] = Sum[H, Face, X]

  def page(mode: String): String =
    s"""<!DOCTYPE html>
       |<html lang="en"><head><meta charset="utf-8"/><meta name="viewport" content="width=device-width, initial-scale=1"/>
       |<title>Noughts and crosses</title><link rel="stylesheet" href="/okay.css"/></head>
       |<body><div id="app" data-mode="$mode"></div><script src="/app.js"></script></body></html>""".stripMargin
  def script: Either[String, String] = {
    val candidates = sys.props.get("noughts.js").toList ++ List("examples/web/target/scala-3.3.8/okay-faces-examples-web-opt/main.js", "examples/web/target/scala-3.3.8/okay-faces-examples-web-fastopt/main.js")
    candidates.map(new java.io.File(_)).find(_.isFile) match {
      case Some(f) => Right(new String(java.nio.file.Files.readAllBytes(f.toPath), "UTF-8"))
      case None => Left(s"no page script: `sbt -Dplatforms=js examplesWeb/fullLinkJS` makes it, or -Dnoughts.js=path names it (looked at ${candidates.mkString(", ")})")
    }
  }

  def main(args: Array[String]): Unit = {
    val port = args.headOption.filter(_.forall(_.isDigit)).map(_.toInt).getOrElse(8090)
    val host = args.drop(1).headOption.getOrElse("127.0.0.1")
    import Machine._
    val https = new Https[Machine.type]()(Machine)
    val jdk = new JdkHttp[Machine.type]()(Machine)
    val sessions = RestFace(faces).sessions[game.Outcome, H](() => faces.dialogue[game.Outcome, H](faces.steps[game.Outcome, HF, H](game.games[HF]), back = false), vocabulary = game.marks.vocabulary).service
    val service: Request => Response ! H = req => (req.method, req.path) match {
      case ("GET", "/") => pure[Response, H](Response.html(page("remote")))
      case ("GET", "/local") => pure[Response, H](Response.html(page("local")))
      case ("GET", "/okay.css") => pure[Response, H](Response.ok(Css(game.look, game.dark), "text/css; charset=utf-8"))
      case ("GET", "/app.js") => pure[Response, H](script.fold(e => Response.text(e, 503), js => Response.ok(js, "application/javascript; charset=utf-8")))
      case _ => sessions(req)
    }
    val server = JdkServer.start(port, host)
    System.err.println(s"noughts at http://$host:${server.port}/ (the game on the server, the remote face), /local (the game in the browser)")
    script.left.foreach(System.err.println)
    Machine.run(jdk.jdk(server)(https.serve[H](service)))
  }
}
