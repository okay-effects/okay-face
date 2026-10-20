package okay.face.mcp

import okay.effects.Runner
import okay.effects.json.Json
import okay.effects.shape.{JsonSchema, Schema}
import okay.face.{Action, Chosen, Faces, Rejected, Step, View}
import okay.mcp.{Content, ResourceContent, ResourceSpec, Servers, ToolCall, ToolResult, ToolSpec}

/**
 * THE MCP FACE: the dialogue projection. The agent sees, at each step, exactly the actions a person would — as tools,
 * each with its parameters' JSON Schema — plus `look` (the current view) and `back`. A tool call is `resume`; the
 * result is the next view, as Markdown text and as structured content; a rejected call is an error result and the step
 * is unchanged. The view is a resource too. The step machine's rest runs by the `Runner`: the row is done before the
 * face, but for `Link`, which the server's loop needs (and `Consent` by elicitation performs).
 */
class McpFace[F <: Runner](val faces: Faces[F]) {
  import faces.fx._
  /** the server over the SAME implementation value, so one `!` serves both */
  val servers: Servers[faces.fx.type] = new Servers[faces.fx.type]()(faces.fx)

  /** one agent's session: the step it is at, the history for `back`, what was told on the way. The rest of a step is a
   * program of the row `R` the server runs in — `Link` among its effects, so a handler inside the program (a consent by
   * elicitation) may speak to the client */
  final class Dialogue[A, R[_]](first: Step[A, ({ type L[X] = X ! R })#L] ! R) {
    type M[X] = X ! R
    private var told: Vector[View] = Vector.empty
    private var history: List[Step.Waiting[A, M]] = Nil
    private var step: Option[Step[A, M]] = None
    private def settle(s: Step[A, M]): Step[A, M] ! R = s match {
      case Step.Told(view, next) => told = told :+ view; next.flatMap(settle)
      case other => pure[Step[A, M], R](other)
    }
    /** the step, the first one run when first asked */
    def current: Step[A, M] ! R = step match {
      case Some(s) => pure[Step[A, M], R](s)
      case None => first.flatMap(settle).map { s => step = Some(s); s }
    }
    def view: View ! R = current.map {
      case w: Step.Waiting[A, M] @unchecked => View.Group(None, told :+ w.view)
      case Step.Done(a) => View.Group(None, told :+ View.text(s"done: $a"))
      case Step.Told(v, _) => v
    }
    def actions: List[Action[_]] ! R = current.map { case w: Step.Waiting[A, M] @unchecked => w.actions; case _ => Nil }
    /** the choice taken: the next view, or the problems */
    def choose(chosen: Chosen): Either[Rejected, View] ! R = current.flatMap {
      case w: Step.Waiting[A, M] @unchecked => w.resume(chosen) match {
        case Left(r) => pure[Either[Rejected, View], R](Left(r))
        case Right(next) => { told = Vector.empty; next.flatMap(settle).flatMap { s => history = w :: history; step = Some(s); view.map(Right(_)) } }
      }
      case _ => pure[Either[Rejected, View], R](Left(Rejected(List(okay.effects.shape.Problem(Nil, "the dialogue is over")))))
    }
    def back(): Boolean = history match {
      case previous :: older => told = Vector.empty; step = Some(previous); history = older; true
      case Nil => false
    }
  }

  /** the dialogue as tools: the step's actions, `look`, `back` */
  def tools[A, R[_]](d: Dialogue[A, R]): servers.Tools[R] = new servers.Tools[R] {
    def list: List[ToolSpec] ! R = d.actions.map(as =>
      as.map(a => ToolSpec(a.name, a.doc.getOrElse(a.label), JsonSchema.of(a.params.erased))) ++ List(
        ToolSpec("look", "the current view, as it stands", Json.obj("type" -> Json.Str("object"))),
        ToolSpec("back", "one step back: the previous view and its actions", Json.obj("type" -> Json.Str("object")))))
    def call(c: ToolCall): ToolResult ! R = c.name match {
      case "look" => d.view.flatMap(v => d.actions.map(as => shown(v, as)))
      case "back" => if (d.back()) d.view.flatMap(v => d.actions.map(as => shown(v, as))) else pure[ToolResult, R](ToolResult.error("nothing to go back to"))
      case name => d.choose(Chosen(name, c.arguments)).flatMap {
        case Right(view) => d.actions.map(as => shown(view, as))
        case Left(Rejected(problems)) => pure[ToolResult, R](ToolResult(List(Content.Text("not taken:\n" + problems.map(p => "- " + p.render).mkString("\n"))),
          Some(Json.obj("problems" -> Json.Arr(problems.map(p => Json.obj("path" -> Json.Arr(p.path.map(Json.Str(_)).toVector), "message" -> Json.Str(p.message))).toVector))), isError = true))
      }
    }
  }
  private def shown(view: View, actions: List[Action[_]]): ToolResult =
    ToolResult(List(Content.Text(Markdown.view(view) + (if (actions.isEmpty) "" else "\n\nactions: " + actions.map(_.name).mkString(", ")))),
      Some(Json.obj("view" -> ViewJson.encode(view), "actions" -> Json.Arr(actions.map(a => Json.Str(a.name)).toVector))))

  /** the current view as a resource, Markdown and JSON */
  def resources[A, R[_]](d: Dialogue[A, R]): servers.Resources[R] = new servers.Resources[R] {
    def list: List[ResourceSpec] ! R = pure[List[ResourceSpec], R](List(
      ResourceSpec("face://view", "the current view", Some("text/markdown")), ResourceSpec("face://view.json", "the current view, structured", Some("application/json"))))
    def read(uri: String): List[ResourceContent] ! R = d.view.map(v => uri match {
      case "face://view" => List(ResourceContent(uri, "text/markdown", Markdown.view(v)))
      case "face://view.json" => List(ResourceContent(uri, "application/json", Json.print(ViewJson.encode(v))))
      case _ => Nil
    })
  }

  /** a server of the dialogue, named; `first` the steps over the server's own row */
  def server[A, R[_]](name: String, version: String, first: Step[A, ({ type L[X] = X ! R })#L] ! R, instructions: Option[String] = None, elicitation: Boolean = false): servers.Server[R] = {
    val d = new Dialogue[A, R](first)
    servers.Server[R](name, version, tools[A, R](d), Some(resources[A, R](d)), instructions, elicitation)
  }
}

/** a `View` as Markdown */
object Markdown {
  def view(v: View): String = v match {
    case View.Empty => ""
    case View.Text(s) => s
    case View.Heading(level, s) => "#" * level + " " + s
    case View.Notice(level, s) => s"**${level.toString.toLowerCase}:** $s"
    case View.Fields(rows) => rows.map { case (k, x) => s"- **$k:** ${oneLine(x)}" }.mkString("\n")
    case View.Table(columns, rows) =>
      val header = if (columns.forall(_.isEmpty)) columns.indices.map(i => s"c${i + 1}") else columns
      (List(header.mkString("| ", " | ", " |"), header.map(_ => "---").mkString("| ", " | ", " |")) ++ rows.map(_.map(oneLine).mkString("| ", " | ", " |"))).mkString("\n")
    case View.Items(items) => items.map(i => "- " + oneLine(i)).mkString("\n")
    case View.Group(title, children) => (title.map("### " + _).toList ++ children.map(view).filter(_.nonEmpty)).mkString("\n\n")
    case View.Ref(key, label) => s"[$label](#$key)"
    case View.Datum(json, _) => "```json\n" + Json.pretty(json) + "\n```"
    case View.Masked(hint) => hint
  }
  def oneLine(v: View): String = v match {
    case View.Fields(rows) => rows.map { case (k, x) => s"$k: ${oneLine(x)}" }.mkString(", ")
    case View.Table(_, rows) => rows.map(_.map(oneLine).mkString(" ")).mkString(" / ")
    case View.Items(items) => items.map(oneLine).mkString(", ")
    case View.Group(t, cs) => (t.toList ++ cs.map(oneLine)).mkString(" ")
    case View.Datum(json, _) => Json.print(json)
    case View.Heading(_, s) => s
    case other => view(other)
  }
}

/** a `View` as JSON, for structured content: `{"type": ..., ...}` */
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
    case View.Datum(json, schema) => Json.Obj(Vector("type" -> Json.Str("datum"), "value" -> json) ++ schema.map(s => "schema" -> Schema.shape.encode(s)))
    case View.Masked(hint) => Json.obj("type" -> Json.Str("masked"), "hint" -> Json.Str(hint))
  }
}
