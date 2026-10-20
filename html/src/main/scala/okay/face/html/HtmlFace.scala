package okay.face.html

import okay.effects.Effects
import okay.effects.json.Json
import okay.effects.shape.Schema
import okay.face.{Action, Chosen, Css, Faces, Look, Rejected, Role, View}
import okay.http.{Query, Request, Response}
import okay.http.rest.{Rests, Trouble}

/**
 * THE HTML FACE: pages from the server, no script. `GET /` opens a session and redirects to it; `GET /s/{id}` the page —
 * the view as HTML, a plain action a button, an action with parameters a form drawn from its erased schema, back a
 * button; `POST /s/{id}/act/{name}` with the form's fields is `resume`, then a redirect to the page (post, redirect, get);
 * a rejected choice shows the page with the problems. What is typed becomes JSON along the schema; the shape validates.
 * The markup is structure alone — HTML5 that is well-formed XML, each part marked with its `Role`'s classes — and the
 * look is the stylesheet at `GET /okay.css`, compiled from the `Look` given.
 */
class HtmlFace[F <: Effects, Fs <: Faces[F]](val faces: Fs) {
  import faces.fx._
  val rests: Rests[faces.fx.type] = new Rests[faces.fx.type]()(faces.fx)
  import rests.{Route, dispatch, get, post}

  final class Pages[A, R[_]](open: () => faces.Dialogue[A, R], title: String, base: String = "/s", look: Look = Look.default) {
    private val sessions = scala.collection.mutable.Map.empty[String, (faces.Dialogue[A, R], List[okay.effects.shape.Problem])]
    private var count = 0
    private def fresh(): String = { count += 1; s"s$count" }
    val stylesheet: String = Css(look)

    def page(id: String, d: faces.Dialogue[A, R], problems: List[okay.effects.shape.Problem]): String ! R =
      d.view.flatMap(v => d.actions.map(as => Html.page(title, v, as, problems, s"$base/$id", d.canGoBack)))

    def routes: List[Route[R]] = List(
      get[R]("/") { (_, _) => val id = fresh(); sessions(id) = (open(), Nil); pure[Response, R](Response.redirect(s"$base/$id")) },
      get[R](Html.stylesheetPath) { (_, _) => pure[Response, R](Response.ok(stylesheet, "text/css; charset=utf-8")) },
      get[R](s"$base/:id") { (ps, _) => sessions.get(ps("id")) match {
        case None => pure[Response, R](Response.html(Html.lost(title, base), 404))
        case Some((d, problems)) => page(ps("id"), d, problems).map(Response.html(_))
      } },
      post[R](s"$base/:id/act/:name") { (ps, req) => sessions.get(ps("id")) match {
        case None => pure[Response, R](Response.html(Html.lost(title, base), 404))
        case Some((d, _)) => d.actions.flatMap { as =>
          val args = as.find(_.name == ps("name")).map(a => Forms.collect(a.params.erased, Query.parse(req.body))).getOrElse(Json.Obj.empty)
          d.choose(Chosen(ps("name"), args)).map {
            case Right(_) => sessions(ps("id")) = (d, Nil); Response.redirect(s"$base/${ps("id")}")
            case Left(Rejected(problems)) => sessions(ps("id")) = (d, problems); Response.redirect(s"$base/${ps("id")}")
          }
        }
      } },
      post[R](s"$base/:id/back") { (ps, _) => sessions.get(ps("id")) match {
        case None => pure[Response, R](Response.html(Html.lost(title, base), 404))
        case Some((d, _)) => d.back(); sessions(ps("id")) = (d, Nil); pure[Response, R](Response.redirect(s"$base/${ps("id")}"))
      } })
    def service: Request => Response ! R = dispatch[R](routes)
  }
  def pages[A, R[_]](open: () => faces.Dialogue[A, R], title: String, base: String = "/s", look: Look = Look.default): Pages[A, R] =
    new Pages[A, R](open, title, base, look)
}

/** THE PAGE: structure only, well-formed — every element closed, void elements self-closed, attributes quoted, text
 * escaped — and a class per role. No style inline, no script: the stylesheet is linked */
object Html {
  val stylesheetPath = "/okay.css"
  def esc(s: String): String = s.flatMap { case '<' => "&lt;"; case '>' => "&gt;"; case '&' => "&amp;"; case '"' => "&quot;"; case c => c.toString }
  private def cls(role: Role): String = s"""class="${role.classes}""""

  def page(title: String, v: View, actions: List[Action[_]], problems: List[okay.effects.shape.Problem], here: String, back: Boolean): String =
    document(title,
      s"""<main ${cls(Role.Face)}>
         |<section ${cls(Role.View)}>${view(v)}</section>
         |${if (problems.isEmpty) "" else s"<ul ${cls(Role.Problems)}>" + problems.map(p => s"<li>${esc(p.render)}</li>").mkString + "</ul>"}
         |<nav ${cls(Role.Actions)}>${actions.map(a => Forms.action(a, here)).mkString("\n")}
         |${if (back) s"""<form method="post" action="$here/back"><button ${cls(Role.Back)}>Back</button></form>""" else ""}</nav>
         |</main>""".stripMargin)
  def lost(title: String, base: String): String =
    document(title, s"""<main ${cls(Role.Face)}><p ${cls(Role.Notice(View.Notice.Problem))}>No such session.</p><nav ${cls(Role.Actions)}><a ${cls(Role.Ref)} href="/">Start again</a></nav></main>""")

  def document(title: String, body: String): String =
    s"""<!DOCTYPE html>
       |<html lang="en"><head><meta charset="utf-8"/><meta name="viewport" content="width=device-width, initial-scale=1"/>
       |<title>${esc(title)}</title><link rel="stylesheet" href="$stylesheetPath"/></head>
       |<body>$body</body></html>""".stripMargin

  def view(v: View): String = v match {
    case View.Empty => ""
    case View.Text(s) => s"<p ${cls(Role.Text)}>${esc(s)}</p>"
    case View.Heading(level, s) => val h = s"h${level min 3 max 1}"; s"<$h ${cls(Role.Heading(level))}>${esc(s)}</$h>"
    case View.Notice(level, s) => s"<p ${cls(Role.Notice(level))}>${esc(s)}</p>"
    case View.Fields(rows) => s"<dl ${cls(Role.Fields)}>" + rows.map { case (k, x) => s"<dt ${cls(Role.Label)}>${esc(k)}</dt><dd>${view(x)}</dd>" }.mkString + "</dl>"
    case View.Table(columns, rows) =>
      val head = if (columns.forall(_.isEmpty)) "" else "<thead><tr>" + columns.map(c => s"<th>${esc(c)}</th>").mkString + "</tr></thead>"
      s"<table ${cls(Role.Table)}>$head<tbody>" + rows.map(r => "<tr>" + r.map(c => s"<td>${view(c)}</td>").mkString + "</tr>").mkString + "</tbody></table>"
    case View.Items(items) => s"<ul ${cls(Role.Items)}>" + items.map(i => s"<li>${view(i)}</li>").mkString + "</ul>"
    case View.Group(title, children) =>
      // a dialogue's stack: what was told above the current view, the last child
      val told = title.isEmpty && children.size > 1
      s"<section ${cls(Role.Group)}>" + title.map(t => s"<h4 ${cls(Role.GroupTitle)}>${esc(t)}</h4>").getOrElse("") +
        children.map(c => if (told && c != children.last) s"<section ${cls(Role.Told)}>${view(c)}</section>" else view(c)).mkString + "</section>"
    case View.Ref(key, label) => s"""<a ${cls(Role.Ref)} href="#${esc(key)}">${esc(label)}</a>"""
    case View.Datum(json, _) => s"<pre ${cls(Role.Datum)}>${esc(Json.pretty(json))}</pre>"
    case View.Masked(hint) => s"<span ${cls(Role.Masked)}>${esc(hint)}</span>"
  }
}

/** an action as a button or a form from its erased schema; a form's fields back to JSON along the schema */
object Forms {
  import Html.esc
  private def cls(role: Role): String = s"""class="${role.classes}""""
  def action[A](a: Action[A], here: String): String = a.params.erased match {
    case Schema.Record(fields) if fields.isEmpty =>
      s"""<form method="post" action="$here/act/${a.name}"><button ${cls(Role.Action)} name="action" value="${a.name}">${esc(a.label)}</button></form>"""
    case schema =>
      s"""<form method="post" ${cls(Role.Form)} action="$here/act/${a.name}">${a.doc.map(d => s"<p ${cls(Role.Doc)}>${esc(d)}</p>").getOrElse("")}${field(Nil, schema, None)}<button ${cls(Role.Action)}>${esc(a.label)}</button></form>"""
  }
  private def name(path: List[String]) = path.mkString(".")
  def field(path: List[String], schema: Schema, f: Option[Schema.Field]): String = schema match {
    case Schema.Record(fs) => "<fieldset>" + f.flatMap(_.label).orElse(f.map(_.name)).map(l => s"<legend ${cls(Role.Label)}>${esc(l)}</legend>").getOrElse("") +
      fs.map(x => field(path :+ x.name, x.schema, Some(x))).mkString + "</fieldset>"
    case Schema.Described(of, label, doc) => field(path, of, Some(f.getOrElse(Schema.Field(path.lastOption.getOrElse(""), of, None, None, false, None)).copy(label = Some(label), doc = doc)))
    case Schema.Optional(of) => field(path, of, f)
    case Schema.Choice(options) => labelled(path, f, s"""<select name="${name(path)}" id="f-${name(path)}">${options.map(o => s"""<option value="${esc(o)}">${esc(o)}</option>""").mkString}</select>""")
    case Schema.Sum(cases) =>
      labelled(path, f, s"""<select name="${name(path)}.type" id="f-${name(path)}">${cases.map { case (n, _) => s"""<option value="${esc(n)}">${esc(n)}</option>""" }.mkString}</select>""") +
        cases.map { case (n, cs) => s"""<div class="okay-case" data-case="${esc(n)}">${field(path :+ n, cs, None)}</div>""" }.mkString
    case Schema.Many(_, _, _) => labelled(path, f, s"""<textarea name="${name(path)}" id="f-${name(path)}" placeholder="one per line"></textarea>""")
    case Schema.Bool => labelled(path, f, s"""<input type="checkbox" name="${name(path)}" id="f-${name(path)}" value="true"${f.flatMap(_.default).collect { case Json.Bool(true) => """ checked="checked"""" }.getOrElse("")}/>""")
    case leaf =>
      val typ = leaf match { case Schema.Integer(_) | Schema.Number(_) => "number"; case Schema.Date => "date"; case _ => if (f.exists(_.secret)) "password" else "text" }
      val default = f.flatMap(_.default).map { case Json.Str(s) => s; case other => Json.print(other) }.map(d => s""" value="${esc(d)}"""").getOrElse("")
      labelled(path, f, s"""<input type="$typ" name="${name(path)}" id="f-${name(path)}"$default/>""")
  }
  private def labelled(path: List[String], f: Option[Schema.Field], control: String): String = {
    val words = f.flatMap(_.label).orElse(f.map(_.name)).orElse(path.lastOption).getOrElse("value")
    s"""<div ${cls(Role.Field)}><label ${cls(Role.Label)} for="f-${name(path)}">${esc(words)}</label>$control${f.flatMap(_.doc).map(d => s"<small ${cls(Role.Doc)}>${esc(d)}</small>").getOrElse("")}</div>"""
  }

  /** the form's fields, by dotted name, to JSON along the schema: blank is absent, a checkbox absent is false */
  def collect(schema: Schema, form: Map[String, List[String]]): Json = {
    def value(path: List[String]): Option[String] = form.get(name(path)).flatMap(_.headOption).filter(_.nonEmpty)
    def go(path: List[String], s: Schema): Json = s match {
      case Schema.Record(fs) => Json.Obj(fs.flatMap(f => go(path :+ f.name, f.schema) match { case Json.Null => None; case v => Some(f.name -> v) }))
      case Schema.Described(of, _, _) => go(path, of)
      case Schema.Optional(of) => go(path, of)
      case Schema.Choice(_) => value(path).map(Json.Str(_)).getOrElse(Json.Null)
      case Schema.Sum(cases) => form.get(name(path) + ".type").flatMap(_.headOption).flatMap(n => cases.find(_._1 == n)) match {
        case Some((n, cs)) => go(path :+ n, cs) match {
          case o: Json.Obj if Schema.isRecord(cs) => ("type" -> Json.Str(n)) +: o
          case v => Json.Obj.of("type" -> Json.Str(n), "value" -> v)
        }
        case None => Json.Null
      }
      case Schema.Many(of, _, _) => value(path).map(t => Json.Arr(t.split("\n").toVector.map(_.trim).filter(_.nonEmpty).map(typed(of, _)))).getOrElse(Json.Null)
      case Schema.Bool => Json.Bool(form.get(name(path)).exists(_.exists(v => Set("true", "on", "yes").contains(v.toLowerCase))))
      case leaf => value(path).map(typed(leaf, _)).getOrElse(Json.Null)
    }
    go(Nil, schema)
  }
  private def typed(schema: Schema, s: String): Json = schema match {
    case Schema.Integer(_) | Schema.Number(_) => scala.util.Try(BigDecimal(s.trim)).toOption.fold[Json](Json.Str(s))(Json.Num(_))
    case Schema.Bool => Json.Bool(Set("true", "on", "yes").contains(s.trim.toLowerCase))
    case Schema.Described(of, _, _) => typed(of, s)
    case _ => Json.Str(s)
  }
}

object HtmlFace {
  /** over these faces, their path kept: `HtmlFace(faces)` — so a `Dialogue` made by `faces` is one this face takes */
  def apply[F <: Effects](faces: Faces[F]): HtmlFace[F, faces.type] = new HtmlFace[F, faces.type](faces)
}
