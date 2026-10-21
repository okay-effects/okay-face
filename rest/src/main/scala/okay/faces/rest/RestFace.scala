package okay.faces.rest

import okay.effects.{Effects, Runner}
import okay.effects.tagless.{Program, Tagless}
import okay.effects.json.Json
import okay.effects.shape.JsonSchema
import okay.faces.{Action, Chosen, Face, Faces, Look, Rejected, Step, View, ViewJson, Vocabulary}
import okay.http.{Http, Https, Request, Response}
import okay.http.rest.{Link, Resource, Rests, Trouble}

/**
 * THE REST FACE: the dialogue as a JSON API in HATEOAS's manner — a representation with its affordances. `POST /sessions`
 * opens a session and answers its first resource; `GET /sessions/{id}` the current view and the actions as links, each
 * with its method, its href and its parameters' JSON Schema; `POST /sessions/{id}/act/{name}` with the arguments is
 * `resume` — the next resource, or a 400 problem with the step unchanged; `POST /sessions/{id}/back`. Sessions live in
 * memory, by id. Every face adds two things to the step machine: how it shows, how it reads a choice; this one shows
 * JSON and reads JSON. The data carries the actions as affordances too — the exact `Schema`, for a client of ours such as
 * the remote Laminar face; the links carry JSON Schema for everyone else.
 */
class RestFace[F <: Runner with Singleton, Fs <: Faces[F]](val faces: Fs) {
  import faces.fx._
  val rests: Rests[faces.fx.type] = new Rests[faces.fx.type]()(faces.fx)
  import rests.{Route, dispatch, get, post}

  final class Sessions[A, R[_]](open: () => faces.Dialogue[A, R], base: String = "/sessions", vocabulary: Vocabulary = Vocabulary.empty) {
    private val sessions = scala.collection.mutable.Map.empty[String, faces.Dialogue[A, R]]
    private var count = 0
    private def fresh(): String = { count += 1; s"s$count" }

    def resource(id: String, d: faces.Dialogue[A, R]): Resource ! R = d.view.flatMap(v => d.actions.flatMap(as => d.done.flatMap(over => d.listening.map { listening =>
      val links = as.map(a => Link(a.name, s"$base/$id/act/${a.name}", "POST", Some(a.doc.getOrElse(a.label)), Some(JsonSchema.of(a.params.erased)))) ++
        List(Link("self", s"$base/$id")) ++ (if (d.canGoBack) List(Link("back", s"$base/$id/back", "POST")) else Nil) ++
        (if (vocabulary.isEmpty) Nil else List(Link("marks", s"$base/marks", "GET", Some("the application's marks on views, and what they mean"))))
      Resource(Json.obj("view" -> ViewJson.encode(v), "done" -> Json.Bool(over), "back" -> Json.Bool(d.canGoBack), "listening" -> Json.Bool(listening), "actions" -> Json.Arr(as.map(_.affordance.json).toVector)), links)
    })))
    def routes: List[Route[R]] = List(
      get[R](s"$base/marks") { (_, _) => pure[Response, R](if (vocabulary.isEmpty) Trouble(404, "no marks").response else Response.json(vocabulary.json)) },
      post[R](base) { (_, _) => val id = fresh(); val d = open(); sessions(id) = d; resource(id, d).map(_.response(201).header("Location", s"$base/$id")) },
      get[R](s"$base/:id") { (ps, _) => sessions.get(ps("id")).fold(pure[Response, R](Trouble(404, "no such session").response))(d => resource(ps("id"), d).map(_.response())) },
      post[R](s"$base/:id/act/:name") { (ps, req) => sessions.get(ps("id")) match {
        case None => pure[Response, R](Trouble(404, "no such session").response)
        case Some(d) =>
          val args = if (req.body.trim.isEmpty) Right(Json.Obj.empty) else req.json
          args match {
            case Left(e) => pure[Response, R](Trouble(400, "the body is not JSON", List(okay.effects.shape.Problem(Nil, e))).response)
            case Right(j) => d.choose(Chosen(ps("name"), j)).flatMap {
              case Right(_) => resource(ps("id"), d).map(_.response())
              case Left(Rejected(problems)) => pure[Response, R](Trouble(400, "not taken", problems).response)
            }
          }
      } },
      post[R](s"$base/:id/back") { (ps, _) => sessions.get(ps("id")) match {
        case None => pure[Response, R](Trouble(404, "no such session").response)
        case Some(d) => if (d.back()) resource(ps("id"), d).map(_.response()) else pure[Response, R](Trouble(409, "nothing to go back to").response)
      } })
    def service: Request => Response ! R = dispatch[R](routes)
  }
  def sessions[A, R[_]](open: () => faces.Dialogue[A, R], base: String = "/sessions", vocabulary: Vocabulary = Vocabulary.empty): Sessions[A, R] = new Sessions[A, R](open, base, vocabulary)

  /** THE HANDLER: a program of the face as a JSON API, a service over `Http` — `POST base` opens a session, the resource
   * at `base/{id}` with its links, the marks at `base/marks` — for a server, or the remote face */
  def handle[A](p: Program[A, okay.faces.Face], base: String = "/sessions", vocabulary: Vocabulary = Vocabulary.empty, back: Boolean = true): Request => Response ! H =
    sessions[A, H](() => faces.dialogue[A, H](faces.steps[A, HF, H](faces.of(p).into[HF]), back), base, vocabulary).service
  /** the same served: `run(new RestFace().serve(Noughts.game, port, host))`; with a `page`, the thin page at `/` that plays
   * the sessions from the browser — the remote face's script (`okay.faces.laminar.RemotePage`, linked), knowing no program */
  def serve[A](p: Program[A, okay.faces.Face], port: Int = 8080, host: String = "127.0.0.1", page: Option[() => Either[String, String]] = None, look: Option[Look] = None,
               base: String = "/sessions", vocabulary: Vocabulary = Vocabulary.empty, back: Boolean = true): Unit ! Pure = {
    val title = Step.opening[A, ({ type M[X] = X ! Pure })#M](faces.steps[A, okay.faces.Face, Pure](faces.of(p)), run).title.getOrElse("okay")
    Web.serve(faces.fx, port, host)((List(base -> handle[A](p, base, vocabulary, back)) ++ page.map(script => "/" -> new Shell[faces.fx.type]()(faces.fx).handle(title, look, "/" -> script)).toList): _*)
  }
  type H[X] = okay.effects.Sum[Pure, Http, X]
  type HF[X] = okay.effects.Sum[H, Face, X]
}

object RestFace {
  /** over these faces, their path kept: `RestFace.over(faces)` — so a `Dialogue` made by `faces` is one this face takes */
  def over[F <: Runner with Singleton](faces: Faces[F]): RestFace[F, faces.type] = new RestFace[F, faces.type](faces)
  /** over the faces in scope */
  def apply[F <: Runner with Singleton]()(implicit faces: Faces[F]): RestFace[F, Faces[F]] = new RestFace[F, Faces[F]](faces)   // not `faces.type`: an implicit is no stable path
  /** THE HANDLER, whole, served: `run(RestFace.serve(Noughts.games, port, host))` */
  def serve[F <: Runner with Singleton, A](p: Program[A, okay.faces.Face], port: Int = 8080, host: String = "127.0.0.1", page: Option[() => Either[String, String]] = None, look: Option[Look] = None,
                                           base: String = "/sessions", vocabulary: Vocabulary = Vocabulary.empty, back: Boolean = true)(implicit faces: Faces[F]): Program[Unit, Tagless.Pure] =
    Faces.running[F, Unit](faces)(new RestFace[F, Faces[F]](faces).serve[A](p, port, host, page, look, base, vocabulary, back).asInstanceOf[faces.fx.![Unit, faces.fx.Pure]])
}
