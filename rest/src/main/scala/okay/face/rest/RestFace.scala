package okay.face.rest

import okay.effects.Effects
import okay.effects.json.Json
import okay.effects.shape.JsonSchema
import okay.face.{Action, Chosen, Faces, Rejected, View}
import okay.http.{Http, Https, Request, Response}
import okay.http.rest.{Link, Resource, Rests, Trouble}

/**
 * THE REST FACE: the dialogue as a JSON API in HATEOAS's manner — a representation with its affordances. `POST /sessions`
 * opens a session and answers its first resource; `GET /sessions/{id}` the current view and the actions as links, each
 * with its method, its href and its parameters' JSON Schema; `POST /sessions/{id}/act/{name}` with the arguments is
 * `resume` — the next resource, or a 400 problem with the step unchanged; `POST /sessions/{id}/back`. Sessions live in
 * memory, by id. Every face adds two things to the step machine: how it shows, how it reads a choice; this one shows
 * JSON and reads JSON.
 */
class RestFace[F <: Effects, Fs <: Faces[F]](val faces: Fs) {
  import faces.fx._
  val rests: Rests[faces.fx.type] = new Rests[faces.fx.type]()(faces.fx)
  import rests.{Route, dispatch, get, post}

  final class Sessions[A, R[_]](open: () => faces.Dialogue[A, R], base: String = "/sessions") {
    private val sessions = scala.collection.mutable.Map.empty[String, faces.Dialogue[A, R]]
    private var count = 0
    private def fresh(): String = { count += 1; s"s$count" }

    def resource(id: String, d: faces.Dialogue[A, R]): Resource ! R = d.view.flatMap(v => d.actions.flatMap(as => d.done.map { over =>
      val links = as.map(a => Link(a.name, s"$base/$id/act/${a.name}", "POST", Some(a.doc.getOrElse(a.label)), Some(JsonSchema.of(a.params.erased)))) ++
        List(Link("self", s"$base/$id")) ++ (if (d.canGoBack) List(Link("back", s"$base/$id/back", "POST")) else Nil)
      Resource(Json.obj("view" -> ViewJson.encode(v), "done" -> Json.Bool(over)), links)
    }))
    def routes: List[Route[R]] = List(
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
  def sessions[A, R[_]](open: () => faces.Dialogue[A, R], base: String = "/sessions"): Sessions[A, R] = new Sessions[A, R](open, base)
}

/** a `View` as JSON: `{"type": ..., ...}` */
object ViewJson {
  def encode(v: View): Json = v match {
    case View.Empty => Json.obj("type" -> Json.Str("empty"))
    case View.Text(s) => Json.obj("type" -> Json.Str("text"), "text" -> Json.Str(s))
    case View.Heading(level, s) => Json.obj("type" -> Json.Str("heading"), "level" -> Json.num(level), "text" -> Json.Str(s))
    case View.Notice(level, s) => Json.obj("type" -> Json.Str("notice"), "level" -> Json.Str(level.toString.toLowerCase), "text" -> Json.Str(s))
    case View.Fields(rows) => Json.obj("type" -> Json.Str("fields"), "rows" -> Json.Arr(rows.map { case (k, x) => Json.obj("label" -> Json.Str(k), "value" -> encode(x)) }))
    case View.Table(columns, rows) => Json.obj("type" -> Json.Str("table"), "columns" -> Json.Arr(columns.map(Json.Str(_))), "rows" -> Json.Arr(rows.map(r => Json.Arr(r.map(encode)))))
    case View.Items(items) => Json.obj("type" -> Json.Str("items"), "items" -> Json.Arr(items.map(encode)))
    case View.Group(title, children) => Json.Obj(Vector("type" -> Json.Str("group")) ++ title.map(t => "title" -> Json.Str(t)) :+ ("children" -> Json.Arr(children.map(encode))))
    case View.Ref(key, label) => Json.obj("type" -> Json.Str("ref"), "key" -> Json.Str(key), "label" -> Json.Str(label))
    case View.Datum(json, schema) => Json.Obj(Vector("type" -> Json.Str("datum"), "value" -> json) ++ schema.map(s => "schema" -> okay.effects.shape.Schema.shape.encode(s)))
    case View.Masked(hint) => Json.obj("type" -> Json.Str("masked"), "hint" -> Json.Str(hint))
  }
}

object RestFace {
  /** over these faces, their path kept: `RestFace(faces)` — so a `Dialogue` made by `faces` is one this face takes */
  def apply[F <: Effects](faces: Faces[F]): RestFace[F, faces.type] = new RestFace[F, faces.type](faces)
}
