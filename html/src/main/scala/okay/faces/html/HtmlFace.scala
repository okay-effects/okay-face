package okay.faces.html

import okay.effects.Effects
import okay.effects.tagless.{Program, Tagless}
import okay.effects.json.Json
import okay.effects.shape.Schema
import okay.faces.{Action, Chosen, Css, Face, Faces, Look, Rejected, Role, View}
import okay.http.{Http, Query, Request, Response}
import okay.http.rest.{Rests, Trouble}

/**
 * THE HTML FACE: pages from the server, no script. `GET /` opens a session and redirects to it; `GET /s/{id}` the page
 * —
 * the view as HTML, a plain action a button, an action with parameters a form drawn from its erased schema, back a
 * button; `POST /s/{id}/act/{name}` with the form's fields is `resume`, then a redirect to the page (post, redirect,
 * get);
 * a rejected choice shows the page with the problems. What is typed becomes JSON along the schema; the shape validates.
 * The markup is structure alone — HTML5 that is well-formed XML, each part marked with its `Role`'s classes — and the
 * look is the stylesheet at `GET /okay.css`, compiled from the `Look` given.
 */
class HtmlFace[F <: Effects with Singleton](implicit val fx: F) {
  import fx._
  val faces: Faces[fx.type] = new Faces[fx.type]()(fx)
  val rests: Rests[fx.type] = new Rests[fx.type]()(fx)
  import rests.{Route, dispatch, get, post}

  final class Pages[A, R[_]](open: () => faces.Dialogue[A, R], title: String, base: String = "/s",
    look: Look = Look.default) {
    private val sessions =
      scala.collection.mutable.Map.empty[String, (faces.Dialogue[A, R], List[okay.effects.shape.Problem])]
    private var count = 0
    private def fresh(): String = { count += 1; s"s$count" }
    val stylesheet: String = Css(look)

    def page(id: String, d: faces.Dialogue[A, R], problems: List[okay.effects.shape.Problem]): String ! R =
      d.shown.flatMap { case (told, v) =>
        d.actions.map(as =>
          Html.page(View.title(told :+ v).getOrElse(title), told, v, as, problems, s"$base/$id", d.canGoBack))
      }

    def routes: List[Route[R]] = List(
      get[R]("/") { (_, _) =>
        val id = fresh(); sessions(id) = (open(), Nil); pure[Response, R](Response.redirect(s"$base/$id"))
      },
      // the base itself opens a session too: a service beside others, with `/` taken, has its door at `base`
      get[R](base) { (_, _) =>
        val id = fresh(); sessions(id) = (open(), Nil); pure[Response, R](Response.redirect(s"$base/$id"))
      },
      get[R](Html.stylesheetPath) { (_, _) => pure[Response, R](Response.ok(stylesheet, "text/css; charset=utf-8")) },
      get[R](s"$base/:id") { (ps, _) =>
        sessions.get(ps("id")) match {
          case None => pure[Response, R](Response.html(Html.lost(title, base), 404))
          case Some((d, problems)) => page(ps("id"), d, problems).map(Response.html(_))
        }
      },
      post[R](s"$base/:id/act/:name") { (ps, req) =>
        sessions.get(ps("id")) match {
          case None => pure[Response, R](Response.html(Html.lost(title, base), 404))
          case Some((d, _)) => d.actions.flatMap { as =>
              val args =
                as.find(_.name == ps("name")).map(a => Forms.collect(a.params.erased, Query.parse(req.body))).getOrElse(
                  Json.Obj.empty)
              d.choose(Chosen(ps("name"), args)).map {
                case Right(_) => sessions(ps("id")) = (d, Nil); Response.redirect(s"$base/${ps("id")}")
                case Left(Rejected(problems)) =>
                  sessions(ps("id")) = (d, problems); Response.redirect(s"$base/${ps("id")}")
              }
            }
        }
      },
      // a `Ref` followed: the action `ref` with the key, when the step offers one; else back to the page
      get[R](s"$base/:id/ref/:key") { (ps, _) =>
        sessions.get(ps("id")) match {
          case None => pure[Response, R](Response.html(Html.lost(title, base), 404))
          case Some((d, _)) => d.actions.flatMap { as =>
              if (as.exists(_.name == "ref")) d.choose(Chosen("ref", Json.obj("key" -> Json.Str(ps("key"))))).map {
                case Right(_) => sessions(ps("id")) = (d, Nil); Response.redirect(s"$base/${ps("id")}")
                case Left(Rejected(problems)) =>
                  sessions(ps("id")) = (d, problems); Response.redirect(s"$base/${ps("id")}")
              }
              else pure[Response, R](Response.redirect(s"$base/${ps("id")}"))
            }
        }
      },
      post[R](s"$base/:id/back") { (ps, _) =>
        sessions.get(ps("id")) match {
          case None => pure[Response, R](Response.html(Html.lost(title, base), 404))
          case Some((d, _)) =>
            d.back(); sessions(ps("id")) = (d, Nil); pure[Response, R](Response.redirect(s"$base/${ps("id")}"))
        }
      })
    def service: Request => Response ! R = dispatch[R](routes)
  }
  def pages[A, R[_]](open: () => faces.Dialogue[A, R], title: String, base: String = "/s", look: Look = Look.default)
    : Pages[A, R] =
    new Pages[A, R](open, title, base, look)

  /** THE HANDLER: a program of the face as pages, a service over `Http` — `/` and `base` open a session, the pages at
   * `base/{id}`, titled by the level-1 heading shown, the look at `/okay.css` */
  def handle[A](p: Program[A, okay.faces.Face], base: String = "/s", look: Option[Look] = None, back: Boolean = true)
    : Request => Response ! H =
    pages[A, H](() => faces.dialogue[A, H](faces.steps[A, HF, H](faces.of(p).into[HF]), back), "okay", base,
      look.getOrElse(Look.default)).service
  /** the same served: `run(new HtmlFace().serve(Noughts.games, port, host))` */
  def serve[A](p: Program[A, okay.faces.Face], port: Int = 8080, host: String = "127.0.0.1", look: Option[Look] = None,
    base: String = "/s", back: Boolean = true): Unit ! Pure =
    okay.faces.rest.Web.serve(fx, port, host)("/" -> handle[A](p, base, look, back))
  type H[X] = okay.effects.Sum[Pure, Http, X]
  type HF[X] = okay.effects.Sum[H, Face, X]
}

/** THE PAGE: structure only, well-formed — every element closed, void elements self-closed, attributes quoted, text
 * escaped — and a class per role. No style inline, no script: the stylesheet is linked */
object Html {
  val stylesheetPath = "/okay.css"
  def esc(s: String): String = s.flatMap {
    case '<' => "&lt;"; case '>' => "&gt;"; case '&' => "&amp;"; case '"' => "&quot;"; case c => c.toString
  }
  private def cls(role: Role): String = s"""class="${role.classes}""""

  def page(title: String, v: View, actions: List[Action[_]], problems: List[okay.effects.shape.Problem], here: String,
    back: Boolean): String = page(title, Vector.empty, v, actions, problems, here, back)
  /** the page: what was told since the last step, each in its own `told` section, then the step's view; the refs live
   * when the step offers `ref` */
  def page(title: String, told: Vector[View], v: View, actions: List[Action[_]],
    problems: List[okay.effects.shape.Problem], here: String, back: Boolean): String =
    document(title,
      s"""<main ${cls(Role.Face)}>
         |<section ${cls(Role.View)}>${told.map(t =>
          s"<section ${cls(Role.Told)}>${view(t)}</section>").mkString}${view(
          v, if (actions.exists(_.name == "ref")) Some(here) else None)}</section>
         |${if (problems.isEmpty) ""
        else s"<ul ${cls(Role.Problems)}>" + problems.map(p => s"<li>${esc(p.render)}</li>").mkString + "</ul>"}
         |<nav ${cls(Role.Actions)}>${actions.filterNot(_.name == "ref").map(a => Forms.action(a, here)).mkString("\n")}
         |${if (back) s"""<form method="post" action="$here/back"><button ${cls(Role.Back)}>Back</button></form>"""
        else ""}</nav>
         |</main>""".stripMargin)
  def lost(title: String, base: String): String =
    document(title,
      s"""<main ${cls(Role.Face)}><p ${cls(Role.Notice(View.Notice.Problem))}>No such session.</p><nav ${cls(
          Role.Actions)}><a ${cls(Role.Ref)} href="/">Start again</a></nav></main>""")

  def document(title: String, body: String): String =
    s"""<!DOCTYPE html>
       |<html lang="en"><head><meta charset="utf-8"/><meta name="viewport" content="width=device-width, initial-scale=1"/>
       |<title>${esc(title)}</title><link rel="stylesheet" href="$stylesheetPath"/></head>
       |<body>$body</body></html>""".stripMargin

  def view(v: View): String = view(v, None)
  /** `here`: the page, when the step offers `ref` — a `Ref` is then a link to `here/ref/key` */
  def view(v: View, here: Option[String]): String = v match {
    case View.Empty => ""
    case View.Text(s) => s"<p ${cls(Role.Text)}>${esc(s)}</p>"
    case View.Heading(level, s) => val h = s"h${level min 3 max 1}"; s"<$h ${cls(Role.Heading(level))}>${esc(s)}</$h>"
    case View.Notice(level, s) => s"<p ${cls(Role.Notice(level))}>${esc(s)}</p>"
    case View.Fields(rows) => s"<dl ${cls(Role.Fields)}>" + rows.map { case (k, x) =>
        s"<dt ${cls(Role.Label)}>${esc(k)}</dt><dd>${view(x, here)}</dd>"
      }.mkString + "</dl>"
    case View.Table(columns, rows) =>
      val head = if (columns.forall(_.isEmpty)) ""
      else "<thead><tr>" + columns.map(c => s"<th>${esc(c)}</th>").mkString + "</tr></thead>"
      s"<table ${cls(Role.Table)}>$head<tbody>" + rows.map(r =>
        "<tr>" + r.map(c => s"<td>${view(c, here)}</td>").mkString + "</tr>").mkString + "</tbody></table>"
    case View.Items(items) =>
      s"<ul ${cls(Role.Items)}>" + items.map(i => s"<li>${view(i, here)}</li>").mkString + "</ul>"
    case View.Group(title, children) =>
      s"<section ${cls(Role.Group)}>" + title.map(t => s"<h4 ${cls(Role.GroupTitle)}>${esc(t)}</h4>").getOrElse("") +
        children.map(c => view(c, here)).mkString + "</section>"
    case View.Ref(key, label) => s"""<a ${cls(Role.Ref)} href="${here.fold("#" + esc(key))(h =>
          s"$h/ref/${Query.encode(key)}")}">${esc(label)}</a>"""
    case View.Datum(json, _) => s"<pre ${cls(Role.Datum)}>${esc(Json.pretty(json))}</pre>"
    case View.Masked(hint) => s"<span ${cls(Role.Masked)}>${esc(hint)}</span>"
    case View.Marked(mark, x) => s"<span ${cls(Role.Mark(mark))}>${view(x, here)}</span>"
  }
}

/** an action as a button or a form from its erased schema; a form's fields back to JSON along the schema */
object Forms {
  import Html.esc
  private def cls(role: Role): String = s"""class="${role.classes}""""
  def action[A](a: Action[A], here: String): String = a.params.erased match {
    case Schema.Record(fields) if fields.isEmpty =>
      s"""<form method="post" action="$here/act/${a.name}"><button ${cls(
          Role.Action)} name="action" value="${a.name}">${esc(a.label)}</button></form>"""
    case schema =>
      s"""<form method="post" ${cls(Role.Form)} action="$here/act/${a.name}">${a.doc.map(d =>
          s"<p ${cls(Role.Doc)}>${esc(d)}</p>").getOrElse("")}${field(Nil, schema, None)}<button ${cls(
          Role.Action)}>${esc(a.label)}</button></form>"""
  }
  private def name(path: List[String]) = path.mkString(".")
  def field(path: List[String], schema: Schema, f: Option[Schema.Field]): String = schema match {
    case Schema.Record(fs) => "<fieldset>" + f.flatMap(_.label).orElse(f.map(_.name)).map(l =>
        s"<legend ${cls(Role.Label)}>${esc(l)}</legend>").getOrElse("") +
        fs.map(x => field(path :+ x.name, x.schema, Some(x))).mkString + "</fieldset>"
    case Schema.Described(of, label, doc) => field(path, of,
        Some(f.getOrElse(Schema.Field(path.lastOption.getOrElse(""), of, None, None, false, None)).copy(
            label = Some(label), doc = doc)))
    case Schema.Optional(of) => field(path, of, f)
    case Schema.Choice(options) => labelled(path, f,
        s"""<select name="${name(path)}" id="f-${name(path)}">${options.map(o =>
            s"""<option value="${esc(o)}">${esc(o)}</option>""").mkString}</select>""")
    case Schema.OneOf(cases) =>
      labelled(path, f,
        s"""<select name="${name(path)}.type" id="f-${name(path)}">${cases.map { case (n, _) =>
            s"""<option value="${esc(n)}">${esc(n)}</option>"""
          }.mkString}</select>""") +
        cases.map { case (n, cs) =>
          s"""<div class="okay-case" data-case="${esc(n)}">${field(path :+ n, cs, None)}</div>"""
        }.mkString
    case Schema.Many(of, min, _) if !okay.faces.FormJson.leafish(of) =>
      // a list of records with no script: slots, `min` and one spare, a blank one no item (FormJson.collect)
      val slots = (min max 0) + 1
      s"""<fieldset ${cls(Role.Field)}>""" + f.flatMap(_.label).orElse(f.map(_.name)).map(l =>
        s"<legend ${cls(Role.Label)}>${esc(l)}</legend>").getOrElse("") +
        (0 until slots).map(i =>
          s"""<div class="okay-item" data-item="$i">${field(path :+ i.toString, of,
              None)}</div>""").mkString + "</fieldset>"
    case Schema.Many(_, _, _) => labelled(path, f,
        s"""<textarea name="${name(path)}" id="f-${name(path)}" placeholder="one per line"></textarea>""")
    case Schema.Bool => labelled(path, f,
        s"""<input type="checkbox" name="${name(path)}" id="f-${name(path)}" value="true"${f.flatMap(
            _.default).collect { case Json.Bool(true) => """ checked="checked"""" }.getOrElse("")}/>""")
    case leaf =>
      val typ = leaf match {
        case Schema.Integer(_) | Schema.Number(_) => "number"; case Schema.Date => "date";
        case _ => if (f.exists(_.secret)) "password" else "text"
      }
      val default = f.flatMap(_.default).map { case Json.Str(s) => s; case other => Json.print(other) }.map(d =>
        s""" value="${esc(d)}"""").getOrElse("")
      labelled(path, f, s"""<input type="$typ" name="${name(path)}" id="f-${name(path)}"$default/>""")
  }
  private def labelled(path: List[String], f: Option[Schema.Field], control: String): String = {
    val words = f.flatMap(_.label).orElse(f.map(_.name)).orElse(path.lastOption).getOrElse("value")
    s"""<div ${cls(Role.Field)}><label ${cls(Role.Label)} for="f-${name(path)}">${esc(
        words)}</label>$control${f.flatMap(_.doc).map(d => s"<small ${cls(Role.Doc)}>${esc(d)}</small>").getOrElse(
        "")}</div>"""
  }

  /** the form's fields, by dotted name, to JSON along the schema: core's `FormJson` */
  def collect(schema: Schema, form: Map[String, List[String]]): Json = okay.faces.FormJson.collect(schema, form)
}

object HtmlFace {
  /** over the implementation in scope */
  def apply[F <: Effects with Singleton]()(implicit fx: F): HtmlFace[F] = new HtmlFace[F]()(fx)
  /** THE HANDLER, whole, served: `run(HtmlFace.serve(Noughts.games, port, host))` */
  def serve[F <: okay.effects.Runner with Singleton, A](p: Program[A, okay.faces.Face], port: Int = 8080,
    host: String = "127.0.0.1", look: Option[Look] = None, base: String = "/s", back: Boolean = true)(
    implicit fx: F): Program[Unit, Tagless.Pure] = {
    val face = new HtmlFace[F]()(fx)
    Faces.running(face.fx.run[Unit](face.serve[A](p, port, host, look, base, back)))
  }
}
