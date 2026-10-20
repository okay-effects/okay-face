package okay.face.rest

import okay.effects.Effects
import okay.effects.json.{Descent, Json}
import okay.effects.shape.{JsonSchema, Problem, Schema}
import okay.effects.describe.{Describe, Domains, Op, RemoteFailure}
import okay.face.FormJson
import okay.http.{Fetch, Https, Query, Request, Response}
import okay.http.rest.{Link, Resource, Rests, Trouble}

/**
 * THE DOMAIN PROJECTION ON REST: a described effect served as resources, and performed from afar over `Fetch`. `GET base`
 * is the description — the operations as links, each with its method, href and parameters' JSON Schema. An operation
 * that reads and whose parameters are flat (a record of leaves, optionals and lists of leaves) is a `GET` with the
 * arguments in the query; any other is a `POST` with the arguments as JSON. The result is JSON; parameters that do
 * not decode are a 400 problem. `remote` is the same wire from the client's side: the handler of the effect that
 * calls these resources, so a program performs the effect here and it happens there.
 */
class RestDomains[F <: Effects](implicit implementation: F) {
  val fx: F = implementation
  import fx._
  val domains: Domains[fx.type] = new Domains[fx.type]()(fx)
  val rests: Rests[fx.type] = new Rests[fx.type]()(fx)
  val https: Https[fx.type] = new Https[fx.type]()(fx)
  import rests.{Route, dispatch, route}

  def method[E[_]](o: Op[E, _, _]): String = if (!o.writes && RestDomains.flat(o.params.erased)) "GET" else "POST"

  final class Served[E[_], R[_]](d: Describe[E], base: String)(implicit in: In[E, R]) {
    def description: Resource = Resource(
      Json.obj("name" -> Json.Str(d.name), "operations" -> Json.Arr(d.operations.map(o => Json.Obj(Vector("name" -> Json.Str(o.name), "method" -> Json.Str(method(o)),
        "params" -> Schema.shape.encode(o.params.erased), "result" -> Schema.shape.encode(o.result.erased)) ++ o.doc.map(x => "doc" -> Json.Str(x)))).toVector)),
      Link("self", base) :: d.operations.map(o => Link(o.name, s"$base/${o.name}", method(o), o.doc, Some(JsonSchema.of(o.params.erased)))))
    def routes: List[Route[R]] =
      rests.get[R](base)((_, _) => pure[Response, R](description.response())) ::
        d.operations.map { o =>
          route[R](method(o), s"$base/${o.name}") { (_, req) =>
            val args: Either[Response, Json] =
              if (method(o) == "GET") Right(FormJson.collect(o.params.erased, req.query))
              else if (req.body.trim.isEmpty) Right(Json.Obj.empty)
              else req.json.left.map(e => Trouble(400, "the body is not JSON", List(Problem(Nil, e))).response)
            args match {
              case Left(r) => pure[Response, R](r)
              case Right(j) => domains.serve[E, R](d)(o.name, j).map {
                case Right(result) => Response.json(result)
                case Left(ps) => Trouble(400, "not taken", ps).response
              }
            }
          }
        }
    def service: Request => Response ! R = dispatch[R](routes)
  }
  def serve[E[_], R[_]](d: Describe[E], base: String)(implicit in: In[E, R]): Served[E, R] = new Served[E, R](d, base)

  /** the effect performed over `Fetch` against these resources at `origin` */
  final class Remote[E[_]](d: Describe[E], origin: String, base: String) {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[E, R, Rest], f: In[Fetch, Rest]): A ! Rest =
      domains.remote[E](d).apply[R, Rest, A]({ (o, args) =>
        val url = s"$origin$base/${o.name}"
        val response: Response ! Rest =
          if (method(o) == "GET") { val q = Query.print(FormJson.print(o.params.erased, args)); https.get[Rest](if (q.isEmpty) url else s"$url?$q") }
          else https.postJson[Rest](url, args)
        response.map { r =>
          if (r.status == 200) Descent.parse(r.body).fold(e => throw RemoteFailure(o.name, List(Problem(Nil, e.message))), j => j)
          else throw RemoteFailure(o.name, RestDomains.problems(r))
        }
      })(p)
  }
  def remote[E[_]](d: Describe[E], base: String, origin: String = ""): Remote[E] = new Remote[E](d, origin, base)
}

object RestDomains {
  def apply()(implicit fx: Effects): RestDomains[fx.type] = { implicit val exact: fx.type = fx; new RestDomains[fx.type] }
  /** a record of leaves, optionals and lists of leaves: what a query carries */
  def flat(s: Schema): Boolean = s match {
    case Schema.Record(fs) => fs.forall(f => leafish(f.schema))
    case Schema.Described(of, _, _) => flat(of)
    case _ => false
  }
  private def leafish(s: Schema): Boolean = s match {
    case Schema.Record(_) | Schema.OneOf(_) => false
    case Schema.Described(of, _, _) => leafish(of)
    case Schema.Optional(of) => leafish(of)
    case Schema.Many(of, _, _) => leafish(of) && !Schema.isRecord(of)
    case _ => true
  }
  /** a `Trouble`'s problems, else its status and title */
  def problems(r: Response): List[Problem] = Descent.parse(r.body) match {
    case Right(o: Json.Obj) => o.get("problems") match {
      case Some(Json.Arr(ps)) if ps.nonEmpty => ps.toList.collect { case p: Json.Obj =>
        Problem(p.get("path").collect { case Json.Arr(xs) => xs.toList.collect { case Json.Str(s) => s } }.getOrElse(Nil), p.get("message").collect { case Json.Str(m) => m }.getOrElse("")) }
      case _ => List(Problem(Nil, s"${r.status}: " + o.get("title").collect { case Json.Str(t) => t }.getOrElse(r.body)))
    }
    case _ => List(Problem(Nil, s"${r.status}: ${r.body}"))
  }
}
