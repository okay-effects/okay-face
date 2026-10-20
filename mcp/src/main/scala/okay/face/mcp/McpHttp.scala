package okay.face.mcp

import okay.effects.Effects
import okay.effects.json.{Descent, Json}
import okay.http.{Request, Response}
import okay.mcp.{Rpc, Servers}

/**
 * MCP OVER HTTP: the protocol's "streamable HTTP" transport, the request-and-answer half of it — `POST path` carries
 * one JSON-RPC message or a batch, the answers come back as the response's JSON (a notification alone is `202`);
 * `initialize` opens a session, named by the `Mcp-Session-Id` header the client then sends, each session its own
 * server — its own dialogue; `DELETE path` ends it; `GET path` is `405`: no event stream here, so nothing the server
 * says unasked reaches the client — no `list_changed` (a client re-lists, or takes the single tool), no elicitation
 * (a consent comes another way). On okay-http, so it serves beside the REST and HTML faces on one port.
 */
class McpHttp[F <: Effects, S <: Servers[F]](val servers: S) {
  import servers.fx._

  final class Service[R[_]](open: () => servers.Server[R], path: String) {
    private val sessions = scala.collection.mutable.Map.empty[String, (servers.Server[R], servers.Session)]
    private var count = 0
    private def fresh(): String = { count += 1; s"m$count" }
    private def messages(body: String): Either[Rpc.Failed, List[Rpc]] = Descent.parse(body) match {
      case Left(e) => Left(Rpc.Failed(Json.Null, Rpc.Code.Parse, e.message))
      case Right(Json.Arr(ms)) => ms.toList.foldLeft[Either[Rpc.Failed, List[Rpc]]](Right(Nil))((acc, m) => acc.flatMap(ok => Rpc.decode(Json.print(m)).map(ok :+ _)))
      case Right(m) => Rpc.decode(Json.print(m)).map(List(_))
    }
    private def dispatchAll(server: servers.Server[R], s: servers.Session, ms: List[Rpc]): (servers.Session, List[Rpc]) ! R =
      ms.foldLeft(pure[(servers.Session, List[Rpc]), R]((s, Nil))) { (acc, m) => acc.flatMap { case (s1, out) => servers.dispatch(server, s1, m).map { case (s2, rs) => (s2, out ++ rs) } } }
    /** the answers, the notifications the server made unasked dropped: no stream to carry them */
    private def answered(rs: List[Rpc], batch: Boolean, session: Option[String]): Response = {
      val answers = rs.collect { case a: Rpc.Answer => Rpc.json(a); case f: Rpc.Failed => Rpc.json(f) }
      val r = if (answers.isEmpty) Response(202) else Response.json(if (batch) Json.Arr(answers.toVector) else answers.head)
      session.fold(r)(id => r.header("Mcp-Session-Id", id))
    }
    def service: Request => Response ! R = req => (req.method, req.path) match {
      case ("POST", p) if p == path => messages(req.body) match {
        case Left(f) => pure[Response, R](Response.json(Rpc.json(f), 400))
        case Right(ms) =>
          val batch = req.body.trim.startsWith("[")
          req.header("mcp-session-id") match {
            case Some(id) => sessions.get(id) match {
              case None => pure[Response, R](Response.text("no such session", 404))
              case Some((server, s)) => dispatchAll(server, s, ms).map { case (s2, rs) => sessions(id) = (server, s2); answered(rs, batch, Some(id)) }
            }
            case None if ms.exists { case Rpc.Request(_, "initialize", _) => true; case _ => false } =>
              val id = fresh(); val server = open()
              dispatchAll(server, servers.fresh, ms).map { case (s2, rs) => sessions(id) = (server, s2); answered(rs, batch, Some(id)) }
            case None => pure[Response, R](Response.json(Rpc.json(Rpc.Failed(Json.Null, Rpc.Code.InvalidRequest, "initialize first, then send Mcp-Session-Id")), 400))
          }
      }
      case ("DELETE", p) if p == path => req.header("mcp-session-id").flatMap(id => sessions.remove(id).map(_ => id)) match {
        case Some(_) => pure[Response, R](Response(200))
        case None => pure[Response, R](Response.text("no such session", 404))
      }
      case ("GET", p) if p == path => pure[Response, R](Response.text("no event stream: POST the messages", 405))
      case _ => pure[Response, R](Response.notFound(req.path))
    }
  }
  /** a server per session, at `path` */
  def service[R[_]](open: () => servers.Server[R], path: String = "/mcp"): Service[R] = new Service[R](open, path)
}
object McpHttp {
  /** over these servers, their path kept — an `McpFace`'s, so a `Server` it made is one this carries */
  def apply[F <: Effects](servers: Servers[F]): McpHttp[F, servers.type] = new McpHttp[F, servers.type](servers)
}
