package okay.face.mcp

import okay.effects.Runner
import okay.effects.json.Json
import okay.effects.shape.{JsonSchema, Schema}
import okay.effects.describe.{Describe, Domains}
import okay.face.{Action, Chosen, Faces, Rejected, Step, View}
import okay.mcp.{Content, ResourceContent, ResourceSpec, Servers, ToolCall, ToolResult, ToolSpec}

/**
 * THE MCP FACE: the dialogue projection. The agent sees, at each step, exactly the actions a person would — as tools,
 * each with its parameters' JSON Schema — plus `look` (the current view) and `back`. A tool call is `resume`; the
 * result is the next view, as Markdown text and as structured content; a rejected call is an error result and the step
 * is unchanged. The view is a resource too. The step machine's rest runs by the `Runner`: the row is done before the
 * face, but for `Link`, which the server's loop needs (and `Consent` by elicitation performs).
 */
class McpFace[F <: Runner, Fs <: Faces[F]](val faces: Fs) {
  import faces.fx._
  /** the server over the SAME implementation value, so one `!` serves both */
  val servers: Servers[faces.fx.type] = new Servers[faces.fx.type]()(faces.fx)

  type Dialogue[A, R[_]] = faces.Dialogue[A, R]
  val domains: Domains[faces.fx.type] = new Domains[faces.fx.type]()(faces.fx)

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

  /** THE DOMAIN PROJECTION: a described effect's operations as tools, `{domain}_{op}`, each with its parameters' JSON
   * Schema and its result's; a call performs the operation under the handler in the row and answers the result as JSON.
   * No session, no view */
  def domain[E[_], R[_]](d: Describe[E])(implicit in: In[E, R]): servers.Tools[R] = new servers.Tools[R] {
    def list: List[ToolSpec] ! R = pure[List[ToolSpec], R](d.operations.map(o =>
      ToolSpec(s"${d.name}_${o.name}", o.doc.getOrElse(o.name), JsonSchema.of(o.params.erased), Some(JsonSchema.of(o.result.erased)))))
    def call(c: ToolCall): ToolResult ! R = domains.serve[E, R](d)(c.name.stripPrefix(d.name + "_"), c.arguments).map {
      case Right(j) => ToolResult(List(Content.Text(Json.pretty(j))), Some(j))
      case Left(ps) => ToolResult(List(Content.Text("not taken:\n" + ps.map(p => "- " + p.render).mkString("\n"))), None, isError = true)
    }
  }
  /** two sets of tools as one: a call goes to the first that lists its name */
  def both[R[_]](a: servers.Tools[R], b: servers.Tools[R]): servers.Tools[R] = new servers.Tools[R] {
    def list: List[ToolSpec] ! R = a.list.flatMap(as => b.list.map(as ++ _))
    def call(c: ToolCall): ToolResult ! R = a.list.flatMap(as => if (as.exists(_.name == c.name)) a.call(c) else b.call(c))
  }

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
    val d = faces.dialogue[A, R](first)
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

object McpFace {
  /** over these faces, their path kept: `McpFace(faces)` — so a `Dialogue` made by `faces` is one this face takes */
  def apply[F <: Runner](faces: Faces[F]): McpFace[F, faces.type] = new McpFace[F, faces.type](faces)
}
