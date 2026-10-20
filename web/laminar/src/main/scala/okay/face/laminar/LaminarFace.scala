package okay.face.laminar

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import okay.effects.Runner
import okay.effects.json.Json
import okay.effects.shape.{Problems, Schema}
import okay.face.{Action, Chosen, Faces, Rejected, Step, View}
import org.scalajs.dom

/**
 * THE LAMINAR FACE: a step machine as a Laminar element. The current step is a `Var`; the view is drawn as elements,
 * a plain action is a button, an action with parameters a form drawn from its erased `Schema`; a click or a submit is
 * `resume`, the rest run, the `Var` set — the event loop gets control back between steps, as a browser needs. Back
 * re-resumes the previous `Waiting`. The rest of a step runs by the `Runner`: the row is done before the face.
 */
class LaminarFace[F <: Runner](val faces: Faces[F]) {
  import faces.fx._
  type M[X] = X ! okay.effects.Void

  final case class Shown[A](told: Vector[View], step: Step[A, M], history: List[Step.Waiting[A, M]], problems: Problems)

  /** the element: the machine from its first step, `done` called with the value when the program ends */
  def element[A](first: Step[A, M] ! okay.effects.Void, done: A => Unit = (_: A) => ()): HtmlElement = {
    val state: Var[Shown[A]] = Var(settle(run(first), Vector.empty, Nil))
    def go(next: M[Step[A, M]], history: List[Step.Waiting[A, M]]): Unit = {
      val s = settle(run(next), Vector.empty, history)
      state.set(s)
      s.step match { case Step.Done(a) => done(a); case _ => () }
    }
    div(cls := "okay-face", child <-- state.signal.map(s => render(s, go, state)))
  }

  /** `Told`s run through: their views kept to show above the next step */
  private def settle[A](step: Step[A, M], told: Vector[View], history: List[Step.Waiting[A, M]]): Shown[A] = step match {
    case Step.Told(view, next) => settle(run(next), told :+ view, history)
    case other => Shown(told, other, history, Nil)
  }

  private def render[A](s: Shown[A], go: (M[Step[A, M]], List[Step.Waiting[A, M]]) => Unit, state: Var[Shown[A]]): HtmlElement = {
    val notices = s.told.map(v => div(cls := "okay-told", Views.element(v)))
    s.step match {
      case Step.Done(a) => div(notices, div(cls := "okay-done", s"done: $a"))
      case Step.Told(_, _) => div(notices)
      case w: Step.Waiting[A, M] @unchecked =>
        val problems = if (s.problems.isEmpty) emptyNode else ul(cls := "okay-problems", s.problems.map(p => li(p.render)))
        def choose(chosen: Chosen): Unit = w.resume(chosen) match {
          case Left(Rejected(ps)) => state.set(s.copy(problems = ps))
          case Right(next) => go(next, w :: s.history)
        }
        val back = if (s.history.isEmpty) emptyNode else button(typ := "button", cls := "okay-back", "Back",
          onClick --> Observer[dom.MouseEvent](_ => state.set(Shown(Vector.empty, s.history.head, s.history.tail, Nil))))
        div(notices, div(cls := "okay-view", Views.element(w.view)), problems,
          div(cls := "okay-actions", w.actions.map(a => Forms.action(a, choose)), back))
    }
  }
}

/** a `View` as Laminar elements: semantic tags, classes for a stylesheet, nothing else */
object Views {
  def element(v: View): HtmlElement = v match {
    case View.Empty => emptyNode.asInstanceOf[HtmlElement] match { case _ => span() }
    case View.Text(s) => p(s)
    case View.Heading(1, s) => h1(s)
    case View.Heading(2, s) => h2(s)
    case View.Heading(_, s) => h3(s)
    case View.Notice(level, s) => div(cls := "okay-notice", cls := ("okay-" + level.toString.toLowerCase), s)
    case View.Fields(rows) => dl(rows.flatMap { case (k, x) => List(dt(k), dd(element(x))) })
    case View.Table(columns, rows) => table(
      if (columns.forall(_.isEmpty)) emptyNode else thead(tr(columns.map(c => th(c)))),
      tbody(rows.map(r => tr(r.map(c => td(element(c)))))))
    case View.Items(items) => ul(items.map(i => li(element(i))))
    case View.Group(title, children) => sectionTag(title.map(t => h4(t)), children.map(element))
    case View.Ref(key, label) => a(href := s"#$key", label)
    case View.Datum(json, _) => pre(Json.pretty(json))
    case View.Masked(hint) => span(cls := "okay-masked", hint)
  }
}

/** an action as a button, or as a FORM drawn from its parameters' erased schema: a leaf an input, a record a fieldset,
 * a choice a select, an optional its inner input left blank for none, a list of leaves a textarea one per line. What is
 * typed becomes JSON the action's shape decodes — the face does not validate, the shape does, and the problems come back */
object Forms {
  def action[A](a: Action[A], choose: Chosen => Unit): HtmlElement = a.params.erased match {
    case Schema.Record(fields) if fields.isEmpty =>
      button(typ := "button", cls := "okay-action", dataAttr("action") := a.name, a.label,
        onClick --> Observer[dom.MouseEvent](_ => choose(Chosen.plain(a.name))))
    case schema =>
      val fields = new Inputs
      val body = fields.of(Nil, schema)
      form(cls := "okay-form", dataAttr("action") := a.name, a.doc.map(d => p(cls := "okay-doc", d)), body,
        button(typ := "submit", a.label),
        onSubmit.preventDefault --> Observer[dom.Event](_ => choose(Chosen(a.name, fields.collect(Nil, schema)))))
  }

  /** the inputs of a form, kept by path, read at submit */
  final class Inputs {
    private val inputs = scala.collection.mutable.Map.empty[List[String], ReactiveHtmlElement[dom.html.Input]]
    private val selects = scala.collection.mutable.Map.empty[List[String], ReactiveHtmlElement[dom.html.Select]]
    private val areas = scala.collection.mutable.Map.empty[List[String], ReactiveHtmlElement[dom.html.TextArea]]
    private def id(path: List[String]) = "f-" + path.mkString("-")

    def of(path: List[String], schema: Schema, field: Option[Schema.Field] = None): HtmlElement = schema match {
      case Schema.Record(fs) => fieldSet(field.flatMap(_.label).orElse(field.map(_.name)).map(l => legend(l)),
        fs.map(f => of(path :+ f.name, f.schema, Some(f))))
      case Schema.Described(of0, label0, doc) => of(path, of0, Some(field.getOrElse(Schema.Field(path.lastOption.getOrElse(""), of0, None, None, false, None)).copy(label = Some(label0), doc = doc)))
      case Schema.Optional(of0) => of(path, of0, field)
      case Schema.Choice(options) =>
        val sel = select(idAttr := id(path), options.map(o => option(value := o, o)))
        selects(path) = sel
        labelled(path, field, sel)
      case Schema.Many(_, _, _) =>
        val area = textArea(idAttr := id(path), placeholder := "one per line")
        areas(path) = area
        labelled(path, field, area)
      case Schema.Sum(_) => div(cls := "okay-unsupported", s"${path.mkString(".")}: a sum is not drawn yet")
      case leaf =>
        val in = input(idAttr := id(path), typ := (leaf match {
          case Schema.Integer(_) | Schema.Number(_) => "number"
          case Schema.Bool => "checkbox"
          case Schema.Date => "date"
          case _ => if (field.exists(_.secret)) "password" else "text"
        }), field.flatMap(_.default).map(d => value := (d match { case Json.Str(s) => s; case other => Json.print(other) })))
        inputs(path) = in
        labelled(path, field, in)
    }
    private def labelled(path: List[String], field: Option[Schema.Field], control: HtmlElement): HtmlElement = {
      val words: String = field.flatMap(_.label).orElse(field.map(_.name)).orElse(path.lastOption).getOrElse("value")
      div(cls := "okay-field", label(forId := id(path), words), control, field.flatMap(_.doc).map(d => small(d)))
    }

    /** what was typed, as JSON along the schema; blank is absent */
    def collect(path: List[String], schema: Schema): Json = schema match {
      case Schema.Record(fs) => Json.Obj(fs.flatMap(f => collect(path :+ f.name, f.schema) match { case Json.Null => None; case v => Some(f.name -> v) }))
      case Schema.Described(of0, _, _) => collect(path, of0)
      case Schema.Optional(of0) => collect(path, of0)
      case Schema.Choice(_) => selects.get(path).map(s => Json.Str(s.ref.value)).getOrElse(Json.Null)
      case Schema.Many(of0, _, _) => areas.get(path).map(a => Json.Arr(a.ref.value.split("\n").toVector.map(_.trim).filter(_.nonEmpty).map(l => typed(of0, l)))).getOrElse(Json.Null)
      case Schema.Sum(_) => Json.Null
      case Schema.Bool => inputs.get(path).map(i => Json.Bool(i.ref.checked)).getOrElse(Json.Null)
      case leaf => inputs.get(path).map(_.ref.value).filter(_.nonEmpty).map(typed(leaf, _)).getOrElse(Json.Null)
    }
    private def typed(schema: Schema, s: String): Json = schema match {
      case Schema.Integer(_) | Schema.Number(_) => scala.util.Try(BigDecimal(s.trim)).toOption.fold[Json](Json.Str(s))(Json.Num(_))
      case Schema.Bool => Json.Bool(Set("true", "yes", "y", "on").contains(s.trim.toLowerCase))
      case Schema.Described(of0, _, _) => typed(of0, s)
      case _ => Json.Str(s)
    }
  }
}
