package okay.face.rest

import okay.effects.Effects
import okay.face.{Css, Look, Scenario}
import okay.http.{Http, Request, Response}

/**
 * THE PAGE OF A SCRIPT: a browser cut of a scenario is a Scala.js script — the Laminar face's `mount`, or the remote
 * face's — and the page it runs in is the same for all of them: a root `#app`, the look at `/okay.css`, the script.
 * `Shell.handle(game, "/" -> script)` is a service over `Http` serving each page at its path, its script beside it
 * (`/index.js`, `/local.js`) and the look; `Shell.serve(game, port, host, script)` the whole thing on a port. A script
 * is given as what reads it — `Linked(dir, name)` for one sbt linked — so a server finds its files its own way.
 */
object Shell {
  val stylesheetPath = "/okay.css"
  def esc(s: String): String = s.flatMap { case '<' => "&lt;"; case '>' => "&gt;"; case '&' => "&amp;"; case '"' => "&quot;"; case c => c.toString }
  def page(title: String, script: String): String =
    s"""<!DOCTYPE html>
       |<html lang="en"><head><meta charset="utf-8"/><meta name="viewport" content="width=device-width, initial-scale=1"/>
       |<title>${esc(title)}</title><link rel="stylesheet" href="$stylesheetPath"/></head>
       |<body><div id="app"></div><script src="$script"></script></body></html>""".stripMargin
  /** the script's path for a page's: `/` → `/index.js`, `/local` → `/local.js` */
  def scriptPath(page: String): String = (if (page == "/") "/index" else page.stripSuffix("/")) + ".js"

  def handle[F <: Effects with Singleton, A](s: Scenario[F, A], pages: (String, () => Either[String, String])*): Request => s.faces.fx.![Response, ({ type L[X] = okay.effects.Sum[s.faces.fx.Pure, Http, X] })#L] = {
    import s.faces.fx._
    type H[X] = okay.effects.Sum[Pure, Http, X]
    val stylesheet = Css(s.look.getOrElse(Look.default))
    val scripts = pages.map { case (p, read) => scriptPath(p) -> read }.toMap
    val shown = pages.map { case (p, _) => p -> page(s.title, scriptPath(p)) }.toMap
    req => pure[Response, H]((req.method, req.path) match {
      case ("GET", `stylesheetPath`) => Response.ok(stylesheet, "text/css; charset=utf-8")
      case ("GET", p) if shown.contains(p) => Response.html(shown(p))
      case ("GET", p) if scripts.contains(p) => scripts(p)().fold(e => Response.text(e, 503), js => Response.ok(js, "application/javascript; charset=utf-8"))
      case _ => Response.notFound(req.path)
    })
  }
  /** THE GAME IN THE BROWSER, whole: `run(Shell.serve(game, port, host, Linked(dir, name)))` — the page at `/`, its script, the look */
  def serve[F <: Effects with Singleton, A](s: Scenario[F, A], port: Int, host: String, script: () => Either[String, String]): s.faces.fx.![Unit, s.faces.fx.Pure] =
    Web.serve(s, port, host)("/" -> handle(s, "/" -> script))
}
