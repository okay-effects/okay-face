package okay.face.rest

import okay.effects.Effects
import okay.face.{Css, Look}
import okay.http.{Http, Request, Response}

/**
 * THE PAGE OF A SCRIPT: a browser cut of a program is a Scala.js script — the Laminar face's `mount`, or the remote
 * face's — and the page it runs in is the same for all of them: a root `#app`, the look at `/okay.css`, the script.
 * `new Shell().handle(title, look, "/" -> script)` is a service over `Http` serving each page at its path, its script
 * beside it (`/index.js`, `/local.js`) and the look; `serve` the whole thing on a port. A script is given as what reads
 * it — `Linked(dir, name)` for one sbt linked — so a server finds its files its own way.
 */
class Shell[F <: Effects with Singleton](implicit implementation: F) {
  val fx: F = implementation
  import fx._
  type H[X] = okay.effects.Sum[Pure, Http, X]

  def handle(title: String, look: Option[Look], pages: (String, () => Either[String, String])*): Request => Response ! H = {
    val stylesheet = Css(look.getOrElse(Look.default))
    val scripts = pages.map { case (p, read) => Shell.scriptPath(p) -> read }.toMap
    val shown = pages.map { case (p, _) => p -> Shell.page(title, Shell.scriptPath(p)) }.toMap
    req => pure[Response, H]((req.method, req.path) match {
      case ("GET", Shell.stylesheetPath) => Response.ok(stylesheet, "text/css; charset=utf-8")
      case ("GET", p) if shown.contains(p) => Response.html(shown(p))
      case ("GET", p) if scripts.contains(p) => scripts(p)().fold(e => Response.text(e, 503), js => Response.ok(js, "application/javascript; charset=utf-8"))
      case _ => Response.notFound(req.path)
    })
  }
  /** THE PROGRAM IN THE BROWSER, whole: `run(new Shell().serve("Noughts", port, host, Linked(dir, name)))` — the page at `/`, its script, the look */
  def serve(title: String, port: Int, host: String, script: () => Either[String, String], look: Option[Look] = None): Unit ! Pure =
    Web.serve(fx, port, host)("/" -> handle(title, look, "/" -> script))
}
object Shell {
  /** over the implementation in scope: `run(Shell().serve(title, port, host, Linked(dir, name)))` */
  def apply[F <: Effects with Singleton]()(implicit fx: F): Shell[F] = new Shell[F]()(fx)
  val stylesheetPath = "/okay.css"
  def esc(s: String): String = s.flatMap { case '<' => "&lt;"; case '>' => "&gt;"; case '&' => "&amp;"; case '"' => "&quot;"; case c => c.toString }
  def page(title: String, script: String): String =
    s"""<!DOCTYPE html>
       |<html lang="en"><head><meta charset="utf-8"/><meta name="viewport" content="width=device-width, initial-scale=1"/>
       |<title>${esc(title)}</title><link rel="stylesheet" href="$stylesheetPath"/></head>
       |<body><div id="app"></div><script src="$script"></script></body></html>""".stripMargin
  /** the script's path for a page's: `/` → `/index.js`, `/local` → `/local.js` */
  def scriptPath(page: String): String = (if (page == "/") "/index" else page.stripSuffix("/")) + ".js"
}
