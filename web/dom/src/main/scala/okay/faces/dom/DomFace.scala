package okay.faces.dom

import okay.effects.Runner
import okay.effects.tagless.Program
import okay.effects.async.{Async, Asynchrony}
import okay.effects.json.Json
import okay.effects.shape.{Problems, Schema}
import okay.faces.{Chosen, Css, Faces, Look, Rejected, Role, Step, View, Words}
import org.scalajs.dom
import scala.concurrent.ExecutionContext

/**
 * THE DOM FACE: a step machine in the page by the DOM's own calls, no library between — the same markup as the Laminar
 * face, element for element, class for class, so a look's stylesheet dresses it the same: only the handler changes.
 * The current step is drawn into the face's element; a new step draws it again, a problem or a call out changes only
 * what is shown on it (the problems, the buttons' `disabled`), so what was typed stays. Back re-resumes the previous
 * `Waiting`; with `browserHistory` the browser's back button does the same. The rest of a step runs by the `Runner`
 * when the row is done (`element`), or to a `Future` when `Async` remains (`elementAsync`).
 */
class DomFace[F <: Runner with Singleton](implicit val fx: F) {
  import fx._
  val faces: Faces[fx.type] = new Faces[fx.type]()(fx)

  /** how the rest of a step is run: at once, or when its future completes */
  trait Lower[M[_]] { def apply[X](m: M[X])(k: X => Unit): Unit }

  /** the row done: the rest run at once */
  def element[A](first: Step[A, ({ type L[X] = X ! Pure })#L] ! Pure, done: A => Unit = (_: A) => (),
    settings: DomFace.Settings = DomFace.Settings()): dom.html.Element = {
    type M[X] = X ! Pure
    new Machine[A, M](new Lower[M] { def apply[X](m: M[X])(k: X => Unit): Unit = k(run(m)) }, done, settings)
      .element(first)
  }

  /** THE HANDLER: a program of the face as an element — the dialogue run here, in the browser, with its look as a
   * `<style>` when one is given; `mount` puts it into `#app` or the body: `new DomFace().mount(Noughts.games)` */
  def handle[A](p: Program[A, okay.faces.Face], settings: DomFace.Settings = DomFace.Settings(),
    done: A => Unit = (_: A) => ()): dom.html.Element =
    Html.el("div", null, settings.look.map(DomFace.stylesheet).toList :+
      element[A](faces.steps[A, okay.faces.Face, Pure](faces.of(p)), done, settings): _*)
  def mount[A](p: Program[A, okay.faces.Face], settings: DomFace.Settings = DomFace.Settings(),
    done: A => Unit = (_: A) => ()): Unit = {
    def into(): Unit = {
      val root = Option(dom.document.getElementById("app")).getOrElse(dom.document.body)
      root.appendChild(handle[A](p, settings, done))
    }
    if (dom.document.readyState == "loading") dom.document.addEventListener("DOMContentLoaded", (_: dom.Event) => into())
    else into()
  }

  /** `Async` remaining: the rest run to a `Future` — a program that waits (a fetch, a timer) between steps */
  def elementAsync[A](first: Step[A, ({ type L[X] = X ! Async })#L] ! Async, done: A => Unit = (_: A) => (),
    settings: DomFace.Settings = DomFace.Settings())(implicit ec: ExecutionContext): dom.html.Element = {
    type M[X] = X ! Async
    val asynchrony = new Asynchrony[fx.type]()(fx, ec)
    new Machine[A, M](new Lower[M] { def apply[X](m: M[X])(k: X => Unit): Unit = asynchrony.toFuture(m).foreach(k) },
      done, settings).element(first)
  }

  final case class Shown[A, M[_]](told: Vector[View], step: Step[A, M], history: List[Step.Waiting[A, M]],
    problems: Problems, busy: Boolean)

  /** one machine per element: what is shown, how it runs a rest, what it does at the end */
  final class Machine[A, M[_]](lower: Lower[M], done: A => Unit, settings: DomFace.Settings) {
    private val browserHistory = settings.browserHistory
    private val words = settings.look.map(_.words).getOrElse(Words.english)
    private var shown: Shown[A, M] =
      Shown(Vector.empty, Step.Told(View.Empty, null.asInstanceOf[M[Step[A, M]]]), Nil, Nil, busy = true)
    private val face = Html.el("div", "okay-face")
    // the step drawn, the place of its problems, its buttons that a call out disables
    private var drawn: dom.Node = dom.document.createComment("")
    private var problemsAt: Option[dom.Node] = None
    private val busy = new Forms.Busy(true)
    face.appendChild(drawn)

    def now: Shown[A, M] = shown

    def element(first: M[Step[A, M]]): dom.html.Element = {
      lower(first)(settle(_, Vector.empty, Nil))
      if (browserHistory) {
        lazy val popstate: scala.scalajs.js.Function1[dom.PopStateEvent, Unit] = (_: dom.PopStateEvent) =>
          // the face gone from the page: its listener with it
          if (face.isConnected) back() else dom.window.removeEventListener("popstate", popstate)
        dom.window.addEventListener("popstate", popstate)
      }
      face
    }

    /** a new state: drawn again when the step is another, else its problems and buttons changed in place */
    private def set(s: Shown[A, M]): Unit = {
      val before = shown
      shown = s
      if (!(before.step eq s.step) || !(before.history eq s.history) || !(before.told eq s.told)) {
        val next = render(s)
        face.replaceChild(next, drawn)
        drawn = next
      } else {
        if (before.problems != s.problems) problems(s.problems)
        if (before.busy != s.busy) busy.set(s.busy)
      }
    }

    private def problems(ps: Problems): Unit = problemsAt.foreach { at =>
      val next: dom.Node =
        if (ps.isEmpty) dom.document.createComment("")
        else Html.el("ul", Role.Problems.classes, ps.map(p => Html.el("li", null, Html.text(p.render))): _*)
      at.parentNode.replaceChild(next, at)
      problemsAt = Some(next)
    }

    /** `Told`s run through, their views kept; a `Waiting` or a `Done` is shown */
    def settle(step: Step[A, M], told: Vector[View], history: List[Step.Waiting[A, M]]): Unit = step match {
      case Step.Told(view, next) =>
        val kept = if (view == View.Empty) told else told :+ view
        set(Shown(kept, step, history, Nil, busy = true))
        lower(next)(settle(_, kept, history))
      case w: Step.Waiting[A, M] @unchecked =>
        set(Shown(told, w, history, Nil, busy = false))
        if (browserHistory && history.nonEmpty) dom.window.history.pushState(null, "")
        // the step's event: when it fires and this is still the step, the rest runs as a choice's would
        unlisten();
        unlisten = w.listen { next =>
          val s = shown
          if ((s.step eq w) && !s.busy) {
            set(s.copy(busy = true))
            lower(next)(settle(_, Vector.empty, if (w.back) w :: s.history else s.history))
          }
        }
      case Step.Done(a) =>
        set(Shown(told, step, history, Nil, busy = false)); done(a)
    }
    private var unlisten: () => Unit = () => ()
    def back(): Unit = {
      val s = shown
      if (s.history.nonEmpty && !s.busy) {
        unlisten(); set(Shown(Vector.empty, s.history.head, s.history.tail, Nil, busy = false))
      }
    }
    /** a choice: rejected, the same step with the problems; taken, the rest run */
    def choose(w: Step.Waiting[A, M], chosen: Chosen): Unit = {
      val s = shown
      if (!s.busy) w.resume(chosen) match {
        case Left(Rejected(ps)) => set(s.copy(problems = ps))
        case Right(next) =>
          unlisten(); set(s.copy(busy = true))
          lower(next)(settle(_, Vector.empty, if (w.back) w :: s.history else s.history))
      }
    }

    def render(s: Shown[A, M]): dom.html.Element = {
      val notices = s.told.map(v => Html.el("section", Role.Told.classes, Views.element(v)))
      problemsAt = None
      busy.clear(s.busy)
      s.step match {
        case Step.Done(_) =>
          Html.el("div", null, notices :+ Html.el("div", "okay-done", Views.element(words.over)): _*)
        case Step.Told(_, _) => Html.el("div", null, notices :+ Html.el("div", "okay-busy", Html.text("…")): _*)
        case w: Step.Waiting[A, M] @unchecked =>
          val at: dom.Node = dom.document.createComment("")
          problemsAt = Some(at)
          val ref: Option[String => Unit] = if (w.actions.exists(_.name == "ref"))
            Some(key => choose(w, Chosen("ref", Json.obj("key" -> Json.Str(key)))))
          else None
          val nav = Html.el("nav", Role.Actions.classes,
            w.actions.filterNot(_.name == "ref").map(a => Forms.action(a, choose(w, _), busy, words)): _*)
          if (s.history.nonEmpty) {
            val b = Html.button("button", Role.Back.classes)
            b.appendChild(Html.text(words.back))
            b.addEventListener("click", (_: dom.MouseEvent) => back())
            nav.appendChild(b)
          }
          val step = Html.el("div", null,
            notices ++ Seq(Html.el("section", Role.View.classes, Views.element(w.view, ref)), at, nav): _*)
          problems(s.problems)
          step
      }
    }
  }
}

/** making elements: a tag, its classes, its children — the DOM's own calls */
private[dom] object Html {
  def el(tag: String, cls: String, children: dom.Node*): dom.html.Element = {
    val e = dom.document.createElement(tag).asInstanceOf[dom.html.Element]
    if (cls != null) e.className = cls
    children.foreach(e.appendChild)
    e
  }
  def text(s: String): dom.Text = dom.document.createTextNode(s)
  def button(typ: String, cls: String): dom.html.Button = {
    val b = dom.document.createElement("button").asInstanceOf[dom.html.Button]
    b.setAttribute("type", typ); b.className = cls
    b
  }
}

/** a `View` as elements: semantic tags, classes for a stylesheet, nothing else — the Laminar face's, tag for tag */
object Views {
  import Html.{el, text}
  def element(v: View): dom.html.Element = element(v, None)
  /** `ref`: what a `Ref` does when the step offers the action `ref` — a click chooses it with the key */
  def element(v: View, ref: Option[String => Unit]): dom.html.Element = v match {
    case View.Empty => el("span", null)
    case View.Text(s) => el("p", Role.Text.classes, text(s))
    case View.Heading(1, s) => el("h1", Role.Heading(1).classes, text(s))
    case View.Heading(2, s) => el("h2", Role.Heading(2).classes, text(s))
    case View.Heading(l, s) => el("h3", Role.Heading(l).classes, text(s))
    case View.Notice(level, s) => el("p", Role.Notice(level).classes, text(s))
    case View.Fields(rows) => el("dl", Role.Fields.classes,
        rows.flatMap { case (k, x) => List(el("dt", Role.Label.classes, text(k)), el("dd", null, element(x, ref))) }: _*)
    case View.Table(columns, rows) =>
      val body = el("tbody", null, rows.map(r => el("tr", null, r.map(c => el("td", null, element(c, ref))): _*)): _*)
      if (columns.forall(_.isEmpty)) el("table", Role.Table.classes, body)
      else el("table", Role.Table.classes, el("thead", null, el("tr", null, columns.map(c => el("th", null, text(c))): _*)),
        body)
    case View.Items(items) => el("ul", Role.Items.classes, items.map(i => el("li", null, element(i, ref))): _*)
    case View.Group(title, children) => el("section", Role.Group.classes,
        title.map(t => el("h4", Role.GroupTitle.classes, text(t))).toList ++ children.map(element(_, ref)): _*)
    case View.Ref(key, label) =>
      val a = el("a", Role.Ref.classes)
      a.setAttribute("href", s"#$key"); a.appendChild(text(label))
      ref.foreach(r => a.addEventListener("click", (e: dom.MouseEvent) => { e.preventDefault(); r(key) }))
      a
    // a page ELSEWHERE (another site): a new tab, and the page opened is told nothing of this one; a page of this
    // site (a relative address, `./`): opened in place, as a link within a site is
    case View.Link(label, url) =>
      val a = el("a", Role.Link.classes)
      a.setAttribute("href", url)
      if (url.contains("://")) { a.setAttribute("target", "_blank"); a.setAttribute("rel", "noopener noreferrer") }
      a.appendChild(text(label))
      a
    case View.Datum(json, _) => el("pre", Role.Datum.classes, text(Json.pretty(json)))
    case View.Masked(hint) => el("span", Role.Masked.classes, text(hint))
    case View.Marked(mark, x) => el("span", Role.Mark(mark).classes, element(x, ref))
  }
}

/** an action as a button, or as a FORM drawn from its parameters' erased schema — the Laminar face's, field for
 * field: a leaf an input, a record a fieldset, a choice a select, a sum a select of its cases with the chosen case's
 * fields, an optional its inner input left blank for none, a list of leaves a textarea one per line, a list of records
 * items added and removed by buttons. What is typed becomes JSON the action's shape decodes — the face does not
 * validate, the shape does, and the problems come back */
object Forms {
  import Html.{el, text}

  /** the buttons a call out disables: set at once, and every one again when it changes */
  final class Busy(private var value: Boolean) {
    private var buttons: List[dom.html.Button] = Nil
    def apply(b: dom.html.Button): dom.html.Button = { b.disabled = value; buttons = b :: buttons; b }
    def set(v: Boolean): Unit = { value = v; buttons.foreach(_.disabled = v) }
    /** a step drawn anew: its buttons are new */
    def clear(v: Boolean): Unit = { value = v; buttons = Nil }
  }

  def action[A](a: okay.faces.Action[A], choose: Chosen => Unit, busy: Boolean, words: Words): dom.html.Element =
    action(a, choose, new Busy(busy), words)
  def action[A](a: okay.faces.Action[A], choose: Chosen => Unit, busy: Boolean): dom.html.Element =
    action(a, choose, new Busy(busy), Words.english)
  def action[A](a: okay.faces.Action[A], choose: Chosen => Unit, busy: Busy, words: Words): dom.html.Element =
    a.params.erased match {
      case Schema.Record(fields) if fields.isEmpty =>
        val b = Html.button("button", Role.Action.classes)
        b.setAttribute("data-action", a.name)
        busy(b); b.appendChild(text(a.label))
        b.addEventListener("click", (_: dom.MouseEvent) => choose(Chosen.plain(a.name)))
        b
      case schema =>
        val inputs = new Inputs(words)
        val f = el("form", Role.Form.classes)
        f.setAttribute("data-action", a.name)
        a.doc.foreach(d => f.appendChild(el("p", Role.Doc.classes, text(d))))
        f.appendChild(inputs.of(Nil, schema))
        val submit = Html.button("submit", Role.Action.classes)
        busy(submit); submit.appendChild(text(a.label))
        f.appendChild(submit)
        f.addEventListener("submit", (e: dom.Event) => { e.preventDefault(); choose(Chosen(a.name, inputs.collect(Nil, schema))) })
        f
    }

  /** the inputs of a form, kept by path, read at submit */
  final class Inputs(words: Words = Words.english) {
    private val inputs = scala.collection.mutable.Map.empty[List[String], dom.html.Input]
    private val selects = scala.collection.mutable.Map.empty[List[String], dom.html.Select]
    private val areas = scala.collection.mutable.Map.empty[List[String], dom.html.TextArea]
    private val items = scala.collection.mutable.Map.empty[List[String], () => Vector[Int]]
    private def id(path: List[String]) = "f" + path.map("-" + _).mkString

    def of(path: List[String], schema: Schema, field: Option[Schema.Field] = None): dom.html.Element = schema match {
      case Schema.Record(fs) => el("fieldset", null,
          field.flatMap(_.label).orElse(field.map(_.name)).map(l => el("legend", null, text(l))).toList ++
            fs.map(f => of(path :+ f.name, f.schema, Some(f))): _*)
      case Schema.Described(of0, label0, doc) =>
        of(path, of0,
          Some(field.getOrElse(Schema.Field(path.lastOption.getOrElse(""), of0, None, None, false, None)).copy(
              label = Some(label0), doc = doc)))
      case Schema.Optional(of0) => of(path, of0, field)
      case Schema.Choice(options) =>
        val sel = select(path, options)
        labelled(path, field, sel)
      case Schema.OneOf(cases) =>
        // the case chosen by a select; every case's fields drawn, only the chosen one's shown
        val sel = select(path, cases.map(_._1))
        val shown = cases.map { case (n, cs) =>
          val c = el("div", "okay-case", of(path :+ n, cs))
          c.setAttribute("data-case", n)
          (n, c)
        }
        def show(w: String): Unit = shown.foreach { case (n, c) => c.style.display = if (w == n) "" else "none" }
        show(cases.headOption.map(_._1).getOrElse(""))
        sel.addEventListener("change", (_: dom.Event) => show(sel.value))
        el("div", "okay-sum", labelled(path, field, sel) +: shown.map(_._2): _*)
      case Schema.Many(of0, min, max) if !okay.faces.FormJson.leafish(of0) =>
        // a list of records: an item added by a button, removed by one; each item's fields by index
        var next = 0
        var ids = Vector.empty[Int]
        val legendText = field.flatMap(_.label).orElse(field.map(_.name)).orElse(path.lastOption).getOrElse("items")
        val set = el("fieldset", Role.Field.classes, el("legend", Role.Label.classes, text(legendText)))
        val add = Html.button("button", Role.Action.classes)
        add.setAttribute("data-add", id(path))
        add.appendChild(text(words.add))
        def limited(): Unit = add.disabled = max.exists(ids.size >= _)
        def item(i: Int): Unit = {
          val remove = Html.button("button", Role.Back.classes)
          remove.setAttribute("data-remove", i.toString)
          remove.appendChild(text(words.remove))
          val it = el("div", "okay-item")
          it.setAttribute("data-item", i.toString)
          it.appendChild(of(path :+ i.toString, of0)); it.appendChild(remove)
          remove.addEventListener("click", (_: dom.MouseEvent) => {
            ids = ids.filterNot(_ == i); set.removeChild(it); limited()
          })
          set.insertBefore(it, add)
        }
        def added(): Unit = { ids = ids :+ next; item(next); next += 1; limited() }
        set.appendChild(add)
        (0 until (min max 0)).foreach(_ => added())
        limited()
        add.addEventListener("click", (_: dom.MouseEvent) => added())
        items(path) = () => ids
        set
      case Schema.Many(_, _, _) =>
        val area = dom.document.createElement("textarea").asInstanceOf[dom.html.TextArea]
        area.id = id(path); area.setAttribute("placeholder", words.onePerLine)
        areas(path) = area
        labelled(path, field, area)
      case leaf =>
        val in = dom.document.createElement("input").asInstanceOf[dom.html.Input]
        in.id = id(path)
        in.setAttribute("type", leaf match {
          case Schema.Integer(_) | Schema.Number(_) => "number"
          case Schema.Bool => "checkbox"
          case Schema.Date => "date"
          case _ => if (field.exists(_.secret)) "password" else "text"
        })
        field.flatMap(_.default).foreach(d => in.value = d match { case Json.Str(s) => s; case other => Json.print(other) })
        // A FRACTION IS A NUMBER: without a step a browser takes 1 and refuses 12.5 before the form is sent
        if (leaf match { case Schema.Number(_) => true; case _ => false }) in.setAttribute("step", "any")
        inputs(path) = in
        labelled(path, field, in)
    }
    private def select(path: List[String], options: Seq[String]): dom.html.Select = {
      val sel = dom.document.createElement("select").asInstanceOf[dom.html.Select]
      sel.id = id(path)
      options.foreach { o =>
        val opt = dom.document.createElement("option").asInstanceOf[dom.html.Option]
        opt.value = o; opt.appendChild(text(o)); sel.appendChild(opt)
      }
      selects(path) = sel
      sel
    }
    private def labelled(path: List[String], field: Option[Schema.Field], control: dom.html.Element): dom.html.Element = {
      val words: String = field.flatMap(_.label).orElse(field.map(_.name)).orElse(path.lastOption).getOrElse("value")
      val l = el("label", Role.Label.classes)
      l.setAttribute("for", id(path)); l.appendChild(text(words))
      el("div", Role.Field.classes, l :: control :: field.flatMap(_.doc).map(d => el("small", Role.Doc.classes, text(d)))
        .toList: _*)
    }

    /** what was typed, as JSON along the schema; blank is absent */
    def collect(path: List[String], schema: Schema): Json = schema match {
      case Schema.Record(fs) => Json.Obj(fs.flatMap(f =>
            collect(path :+ f.name, f.schema) match { case Json.Null => None; case v => Some(f.name -> v) }))
      case Schema.Described(of0, _, _) => collect(path, of0)
      case Schema.Optional(of0) => collect(path, of0)
      case Schema.Choice(_) => selects.get(path).map(s => Json.Str(s.value)).getOrElse(Json.Null)
      case Schema.OneOf(cases) => selects.get(path).map(_.value).flatMap(n => cases.find(_._1 == n)) match {
          case Some((n, cs)) => collect(path :+ n, cs) match {
              case o: Json.Obj if Schema.isRecord(cs) => ("type" -> Json.Str(n)) +: o
              case v => Json.Obj.of("type" -> Json.Str(n), "value" -> v)
            }
          case None => Json.Null
        }
      case Schema.Many(of0, _, _) if items.contains(path) =>
        Json.Arr(items(path)().map(i => collect(path :+ i.toString, of0)))
      case Schema.Many(of0, _, _) => areas.get(path).map(a =>
          Json.Arr(a.value.split("\n").toVector.map(_.trim).filter(_.nonEmpty).map(l => typed(of0, l)))).getOrElse(
          Json.Null)
      case Schema.Bool => inputs.get(path).map(i => Json.Bool(i.checked)).getOrElse(Json.Null)
      case leaf => inputs.get(path).map(_.value).filter(_.nonEmpty).map(typed(leaf, _)).getOrElse(Json.Null)
    }
    private def typed(schema: Schema, s: String): Json = schema match {
      case Schema.Integer(_) | Schema.Number(_) =>
        scala.util.Try(BigDecimal(s.trim)).toOption.fold[Json](Json.Str(s))(Json.Num(_))
      case Schema.Bool => Json.Bool(Set("true", "yes", "y", "on").contains(s.trim.toLowerCase))
      case Schema.Described(of0, _, _) => typed(of0, s)
      case _ => Json.Str(s)
    }
  }
}

object DomFace {
  /** THE FACE'S SETTINGS, one value — the Laminar face's: how it looks (a `<style>` and the face's words), whether a
   * step is an entry of the browser's history (its back button then goes back a step) */
  final case class Settings(look: Option[Look] = None, browserHistory: Boolean = false)
  /** over the implementation in scope */
  def apply[F <: Runner with Singleton]()(implicit fx: F): DomFace[F] = new DomFace[F]()(fx)
  /** THE HANDLER, whole, in the page: `DomFace.mount(Noughts.games)` */
  def mount[F <: Runner with Singleton, A](p: Program[A, okay.faces.Face], settings: Settings = Settings(),
    done: A => Unit = (_: A) => ())(implicit fx: F): Unit =
    new DomFace[F]()(fx).mount[A](p, settings, done)
  /** the look as a `<style>` for the document: the same stylesheet the HTML and Laminar faces serve */
  def stylesheet(look: Look = Look.default): dom.html.Element = Html.el("style", null, Html.text(Css(look)))
}
