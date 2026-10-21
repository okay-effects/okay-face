package okay.face.rest

import okay.effects.Effects
import okay.face.Scenario
import okay.http.{Http, Request, Response}

/** no server in the browser: the word is here so the shared source reads alike; calling it is a mistake told at once */
object Web {
  def serve[F <: Effects with Singleton, A](s: Scenario[F, A], port: Int = 8080, host: String = "127.0.0.1", quiet: Boolean = false)(
    mounts: (String, Request => s.faces.fx.![Response, ({ type L[X] = okay.effects.Sum[s.faces.fx.Pure, Http, X] })#L])*): s.faces.fx.![Unit, s.faces.fx.Pure] =
    throw new UnsupportedOperationException("Web.serve: a server runs on the JVM, not in the browser")
}
/** no files in the browser */
final case class Linked(dir: String, name: String) extends (() => Either[String, String]) {
  def apply(): Either[String, String] = Left("Linked: no files in the browser")
}
