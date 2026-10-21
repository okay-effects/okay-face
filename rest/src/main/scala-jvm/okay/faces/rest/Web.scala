package okay.faces.rest

import okay.effects.Effects
import okay.http.{Http, Https, JdkHttp, Request, Response}

/**
 * THE SERVER: `Web.serve(fx, port, host)("/sessions" -> rest, "/" -> pages)` — the JDK's server started when the
 * program
 * runs, on the implementation given, every request to the service of the longest path prefix, until the server stops.
 * The faces' `serve` words are this with one mount.
 */
object Web {
  def serve[F <: Effects with Singleton](fx: F, port: Int = 8080, host: String = "127.0.0.1", quiet: Boolean = false)(
    mounts: (String, Request => fx.![Response, ({ type L[X] = okay.effects.Sum[fx.Pure, Http, X] })#L])*)
    : fx.![Unit, fx.Pure] = {
    val https = new Https[fx.type]()(fx)
    val jdk = new JdkHttp[fx.type]()(fx)
    type H[X] = okay.effects.Sum[fx.Pure, Http, X]
    jdk.serve(port, host, quiet)(https.byPrefix[H](mounts: _*))
  }
}
