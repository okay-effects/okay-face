package okay.face.html

import okay.effects.Effects
import okay.face.Scenario
import okay.http.{Http, Https, JdkHttp, Request, Response}

/**
 * THE SERVER AS A HANDLER: `run(Web.serve(game, port)("/sessions" -> RestFace.handle(game), "/s" -> HtmlFace.handle(game),
 * "/" -> Shell.handle(game, …)))` — the JDK's server started when the program runs, on the scenario's implementation, every
 * request to the service of the longest path prefix, until the server stops. The scenario is the anchor: the services
 * are over its faces, and so is this program.
 */
object Web {
  def serve[F <: Effects with Singleton, A](s: Scenario[F, A], port: Int = 8080, host: String = "127.0.0.1", quiet: Boolean = false)(
    mounts: (String, Request => s.faces.fx.![Response, ({ type L[X] = okay.effects.Sum[s.faces.fx.Pure, Http, X] })#L])*): s.faces.fx.![Unit, s.faces.fx.Pure] = {
    import s.faces.fx._
    type H[X] = okay.effects.Sum[Pure, Http, X]
    val https = new Https[F]()(s.faces.fx)
    val jdk = new JdkHttp[F]()(s.faces.fx)
    type HH[X] = okay.effects.Sum[https.fx.Pure, Http, X]
    // the services are over `s.faces`, the server's words over its own `fx`: one value, two paths the checker cannot join
    val service = https.byPrefix[HH](mounts.map { case (prefix, f) => prefix -> f.asInstanceOf[Request => https.fx.![Response, HH]] }: _*)
    jdk.serve(port, host, quiet)(service.asInstanceOf[Request => jdk.fx.![Response, ({ type L[X] = okay.effects.Sum[jdk.fx.Pure, Http, X] })#L]]).asInstanceOf[Unit ! Pure]
  }
}
