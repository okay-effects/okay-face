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
 * THE DOM FACE: a step machine in the page, no library between — the same markup as the Laminar face, element for
 * element, class for class, so a look's stylesheet dresses it the same: only the handler changes.
 *
 * It is a PROGRAM OVER ITS OWN EFFECTS — `Page` (the DOM), `History` (the browser's back), `Focus` (where the keyboard
 * is), `Keys` (keys listened to) — handled inside it by the browser's handlers (`Browser.world`): the world is reached
 * in one place. A new step is drawn again; a problem or a call out changes only what is shown on it (the problems,
 * the buttons' `disabled`), so what was typed stays. Accessibility by the same effects: the problems a live region,
 * a field with a problem `aria-invalid` and focused, a field's doc its `aria-description`, the face `aria-busy` while
 * a rest runs; Escape is back where the step offers it; a new step's first field focused (`Settings.focus`). The rest
 * of a step runs by the `Runner` when the row is done (`element`), or to a `Future` when `Async` remains
 * (`elementAsync`).
 */
class DomFace[F <: Runner with Singleton](implicit val fx: F) {
  import fx._
  val faces: Faces[fx.type] = new Faces[fx.type]()(fx)
  /** the browser: the handlers of the face's effects, and its row `W` */
  val browser: Browser[fx.type] = new Browser[fx.type]()(fx)
  import browser.W

  /** EVERY PROGRAM OF THE FACE RUNS HERE: by default in the browser (`browser.world`); a face that overrides it may
   * intercept any of its effects first — an audit of what the face does to the page, a test's count */
  def world[X](p: X ! W): X = browser.world(p)

  /** how the rest of a step is run: at once, or when its future completes */
  trait Lower[M[_]] { def apply[X](m: M[X])(k: X => Unit): Unit }

  // ---- THE WORDS OF THE FACE'S EFFECTS

  private def page[X](op: Page[X]): X ! W = perform[Page].apply[W, X](op)
  private def unit: Unit ! W = pure[Unit, W](())
  private def later[X](p: => X ! W): X ! W = delay[X, W](p)
  private def seq[X](ps: List[X ! W]): List[X] ! W =
    ps.foldRight(pure[List[X], W](Nil))((p, acc) => p.flatMap(a => acc.map(a :: _)))
  private def node(n: dom.Node): dom.Node ! W = pure[dom.Node, W](n)
  /** an element: its tag, its attributes in order, its children made first */
  private def el(tag: String, attrs: (String, String)*)(children: List[dom.Node ! W]): dom.html.Element ! W =
    seq(children).flatMap(cs => page(Page.Element[dom.html.Element](tag, attrs.toList, cs, e => e)))
  private def cls(c: String): (String, String) = "class" -> c
  private def txt(s: String): dom.Node ! W = page(Page.Text[dom.Node](s, n => n))
  private def place: dom.Node ! W = page(Page.Place[dom.Node](n => n))
  private def swap(old: dom.Node, next: dom.Node): Unit ! W = page(Page.Swap[Unit](old, next, u => u))
  private def put(n: dom.Node, prop: Page.Prop, v: String): Unit ! W = page(Page.Put[Unit](n, prop, v, u => u))
  private def get(n: dom.Node, prop: Page.Prop): String ! W = page(Page.Get[String](n, prop, s => s))
  private def listen(t: dom.EventTarget, event: String)(act: dom.Event => Unit): Unit ! W =
    page(Page.Listen[Unit](t, event, act, _ => ()))
  private def focusFirst(within: dom.Node, selector: String): Boolean ! W =
    perform[Focus].apply[W, Boolean](Focus.First[Boolean](within, selector, b => b))

  // ---- THE HANDLERS: the same API as the Laminar face

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
    done: A => Unit = (_: A) => ()): dom.html.Element = {
    val face = element[A](faces.steps[A, okay.faces.Face, Pure](faces.of(p)), done, settings)
    world(el("div")(settings.look.toList.map(l => stylesheet(l)) :+ node(face)))
  }
  def mount[A](p: Program[A, okay.faces.Face], settings: DomFace.Settings = DomFace.Settings(),
    done: A => Unit = (_: A) => ()): Unit = {
    def into(): Unit = {
      val root = Option(dom.document.getElementById("app")).getOrElse(dom.document.body)
      val shown = handle[A](p, settings, done)
      world(page(Page.Insert[Unit](root, shown, None, u => u)).flatMap(_ =>
        // in the page now: the first field takes the keyboard
        if (settings.focus) focusFirst(shown, DomFace.fields).map(_ => ()) else unit))
    }
    if (dom.document.readyState == "loading")
      dom.document.addEventListener("DOMContentLoaded", (_: dom.Event) => into())
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

  /** the look as a `<style>`: the same stylesheet the HTML and Laminar faces serve */
  def stylesheet(look: Look = Look.default): dom.Node ! W = el("style")(List(txt(Css(look))))

  final case class Shown[A, M[_]](told: Vector[View], step: Step[A, M], history: List[Step.Waiting[A, M]],
    problems: Problems, busy: Boolean)

  // ---- THE MACHINE: one per element — what is shown, how it runs a rest, what it does at the end

  final class Machine[A, M[_]](lower: Lower[M], done: A => Unit, settings: DomFace.Settings) {
    private val words = settings.look.map(_.words).getOrElse(Words.english)
    private var shown: Shown[A, M] =
      Shown(Vector.empty, Step.Told(View.Empty, null.asInstanceOf[M[Step[A, M]]]), Nil, Nil, busy = true)
    // the face, the step drawn in it, the place of its problems, the buttons a call out disables, its forms
    private var face: dom.html.Element = null
    private var drawn: dom.Node = null
    private var problemsAt: Option[dom.Node] = None
    private var buttons: List[dom.Node] = Nil
    private var forms: List[Inputs] = Nil
    private var unlisten: () => Unit = () => ()
    private def world[X](p: X ! W): X = DomFace.this.world(p)

    def element(first: M[Step[A, M]]): dom.html.Element = {
      world(for {
        f <- el("div", cls("okay-face"), "aria-busy" -> "true")(Nil)
        d <- place
        _ <- page(Page.Insert[Unit](f, d, None, u => u))
        _ <- later { face = f; drawn = d; unit }
        // Escape is back, where the step offers it
        _ <- perform[Keys].apply[W, Unit](Keys.On[Unit](f, "Escape", () => back(), _ => ()))
        _ <- if (settings.browserHistory)
          perform[History].apply[W, Unit](History.Back[Unit](() => back(), () => face.isConnected, _ => ()))
        else unit
      } yield ())
      lower(first)(settle(_, Vector.empty, Nil))
      face
    }

    /** a new state: drawn again when the step is another, else its problems and buttons changed in place */
    private def set(s: Shown[A, M]): Unit ! W = later {
      val before = shown
      shown = s
      if (!(before.step eq s.step) || !(before.history eq s.history) || !(before.told eq s.told))
        render(s).flatMap(next => swap(drawn, next).flatMap(_ => later { drawn = next; busy(s.busy) })).flatMap(_ =>
          s.step match {
            // a new step takes the keyboard where the face is in the page: its first field, else its first action
            case _: Step.Waiting[A, M] @unchecked if settings.focus && face.isConnected =>
              focusFirst(drawn, DomFace.fields).flatMap(found =>
                if (found) unit else focusFirst(drawn, "nav button").map(_ => ()))
            case _ => unit
          })
      else
        (if (before.problems != s.problems) problems(s.problems) else unit).flatMap(_ =>
          if (before.busy != s.busy) busy(s.busy) else unit)
    }

    private def busy(b: Boolean): Unit ! W =
      seq(buttons.map(put(_, Page.Disabled, b.toString))).flatMap(_ =>
        put(face, Page.Attr("aria-busy"), if (b) "true" else ""))

    /** the problems shown at their place, a live region; the fields they are of marked, the first one focused */
    private def problems(ps: Problems): Unit ! W = problemsAt match {
      case None => unit
      case Some(at) =>
        val made: dom.Node ! W =
          if (ps.isEmpty) place
          else el("ul", cls(Role.Problems.classes), "role" -> "alert")(ps.map(p => el("li")(List(txt(p.render)))))
        made.flatMap(next => swap(at, next).flatMap(_ => later { problemsAt = Some(next); unit })).flatMap { _ =>
          val wrong = ps.map(_.path).toSet
          seq(forms.flatMap(_.controls).map { case (path, control) =>
            put(control, Page.Attr("aria-invalid"), if (wrong(path)) "true" else "")
          })
        }.flatMap(_ =>
          if (ps.nonEmpty && settings.focus && face.isConnected) focusFirst(drawn, "[aria-invalid=true]").map(_ => ())
          else unit)
    }

    /** `Told`s run through, their views kept; a `Waiting` or a `Done` is shown */
    def settle(step: Step[A, M], told: Vector[View], history: List[Step.Waiting[A, M]]): Unit = step match {
      case Step.Told(view, next) =>
        val kept = if (view == View.Empty) told else told :+ view
        world(set(Shown(kept, step, history, Nil, busy = true)))
        lower(next)(settle(_, kept, history))
      case w: Step.Waiting[A, M] @unchecked =>
        world(set(Shown(told, w, history, Nil, busy = false)).flatMap(_ =>
          if (settings.browserHistory && history.nonEmpty) perform[History].apply[W, Unit](History.Push[Unit](u => u))
          else unit))
        // the step's event: when it fires and this is still the step, the rest runs as a choice's would
        unlisten()
        unlisten = w.listen { next =>
          val s = shown
          if ((s.step eq w) && !s.busy) {
            world(set(s.copy(busy = true)))
            lower(next)(settle(_, Vector.empty, if (w.back) w :: s.history else s.history))
          }
        }
      case Step.Done(a) =>
        world(set(Shown(told, step, history, Nil, busy = false))); done(a)
    }
    def back(): Unit = {
      val s = shown
      if (s.history.nonEmpty && !s.busy) {
        unlisten(); world(set(Shown(Vector.empty, s.history.head, s.history.tail, Nil, busy = false)))
      }
    }
    /** a choice: rejected, the same step with the problems; taken, the rest run */
    def choose(w: Step.Waiting[A, M], chosen: Chosen): Unit = {
      val s = shown
      if (!s.busy) w.resume(chosen) match {
        case Left(Rejected(ps)) => world(set(s.copy(problems = ps)))
        case Right(next) =>
          unlisten(); world(set(s.copy(busy = true)))
          lower(next)(settle(_, Vector.empty, if (w.back) w :: s.history else s.history))
      }
    }

    def render(s: Shown[A, M]): dom.Node ! W = later {
      problemsAt = None
      buttons = Nil
      forms = Nil
      val notices = s.told.toList.map(v => el("section", cls(Role.Told.classes))(List(view(v, None))))
      s.step match {
        case Step.Done(_) =>
          el("div")(notices :+ el("div", cls("okay-done"))(List(view(words.over, None))))
        case Step.Told(_, _) => el("div")(notices :+ el("div", cls("okay-busy"))(List(txt("…"))))
        case w: Step.Waiting[A, M] @unchecked =>
          val ref: Option[String => Unit] = if (w.actions.exists(_.name == "ref"))
            Some(key => choose(w, Chosen("ref", Json.obj("key" -> Json.Str(key)))))
          else None
          val actions: List[dom.Node ! W] = w.actions.filterNot(_.name == "ref").map(a => action(a, choose(w, _)))
          val backButton: List[dom.Node ! W] =
            if (s.history.isEmpty) Nil
            else List(button("button", Role.Back.classes)(List(txt(words.back))).flatMap(b =>
              listen(b, "click")(_ => back()).map(_ => b)))
          for {
            v <- el("section", cls(Role.View.classes))(List(view(w.view, ref)))
            at <- place
            nav <- el("nav", cls(Role.Actions.classes))(actions ++ backButton)
            step <- el("div")(notices ++ List(node(v), node(at), node(nav)))
            _ <- later { problemsAt = Some(at); problems(s.problems) }
          } yield step
      }
    }

    private def button(typ: String, c: String, attrs: (String, String)*)(children: List[dom.Node ! W])
      : dom.html.Element ! W = el("button", (Seq("type" -> typ, cls(c)) ++ attrs): _*)(children)

    /** an action as a button, or as a form drawn from its parameters' erased schema; what is typed becomes JSON the
     * action's shape decodes — the face does not validate, the shape does, and the problems come back */
    private def action[X](a: okay.faces.Action[X], choose: Chosen => Unit): dom.Node ! W = a.params.erased match {
      case Schema.Record(fields) if fields.isEmpty =>
        button("button", Role.Action.classes, "data-action" -> a.name)(List(txt(a.label))).flatMap { b =>
          later { buttons = b :: buttons; put(b, Page.Disabled, shown.busy.toString) }.flatMap(_ =>
            listen(b, "click")(_ => choose(Chosen.plain(a.name)))).map(_ => b)
        }
      case schema =>
        val inputs = new Inputs
        forms = inputs :: forms
        for {
          submit <- button("submit", Role.Action.classes)(List(txt(a.label)))
          _ <- later { buttons = submit :: buttons; put(submit, Page.Disabled, shown.busy.toString) }
          f <- el("form", cls(Role.Form.classes), "data-action" -> a.name)(
            a.doc.toList.map(d => el("p", cls(Role.Doc.classes))(List(txt(d)))) ++
              List(inputs.of(Nil, schema), node(submit)))
          _ <- listen(f, "submit") { e =>
            e.preventDefault()
            choose(Chosen(a.name, world(inputs.collect(Nil, schema))))
          }
        } yield f
    }

    /** the inputs of a form, kept by path, read at submit */
    final class Inputs {
      private val inputs = scala.collection.mutable.Map.empty[List[String], dom.Node]
      private val selects = scala.collection.mutable.Map.empty[List[String], dom.Node]
      private val areas = scala.collection.mutable.Map.empty[List[String], dom.Node]
      private val items = scala.collection.mutable.Map.empty[List[String], () => Vector[Int]]
      private def id(path: List[String]) = "f" + path.map("-" + _).mkString
      /** the controls by path: where a problem of that path is marked */
      def controls: List[(List[String], dom.Node)] = (inputs ++ selects ++ areas).toList

      def of(path: List[String], schema: Schema, field: Option[Schema.Field] = None): dom.Node ! W = schema match {
        case Schema.Record(fs) => el("fieldset")(
            field.flatMap(_.label).orElse(field.map(_.name)).toList.map(l => el("legend")(List(txt(l)))) ++
              fs.toList.map(f => of(path :+ f.name, f.schema, Some(f))))
        case Schema.Described(of0, label0, doc) =>
          of(path, of0,
            Some(field.getOrElse(Schema.Field(path.lastOption.getOrElse(""), of0, None, None, false, None)).copy(
                label = Some(label0), doc = doc)))
        case Schema.Optional(of0) => of(path, of0, field)
        case Schema.Choice(options) => select(path, options).flatMap(sel => labelled(path, field, sel))
        case Schema.OneOf(cases) =>
          // the case chosen by a select; every case's fields drawn, only the chosen one's shown
          def show(shown: List[(String, dom.Node)], w: String): Unit ! W =
            seq(shown.map { case (n, c) => put(c, Page.Display, if (w == n) "" else "none") }).map(_ => ())
          for {
            sel <- select(path, cases.map(_._1))
            shown <- seq(cases.toList.map { case (n, cs) =>
              el("div", cls("okay-case"), "data-case" -> n)(List(of(path :+ n, cs))).map(c => (n, c: dom.Node))
            })
            _ <- show(shown, cases.headOption.map(_._1).getOrElse(""))
            _ <- listen(sel, "change")(_ => world(get(sel, Page.Value).flatMap(show(shown, _))))
            labelledSel <- labelled(path, field, sel)
            sum <- el("div", cls("okay-sum"))(node(labelledSel) :: shown.map(c => node(c._2)))
          } yield sum
        case Schema.Many(of0, min, max) if !okay.faces.FormJson.leafish(of0) =>
          // a list of records: an item added by a button, removed by one; each item's fields by index
          var next = 0
          var ids = Vector.empty[Int]
          val legendText = field.flatMap(_.label).orElse(field.map(_.name)).orElse(path.lastOption).getOrElse("items")
          def limited(add: dom.Node): Unit ! W = put(add, Page.Disabled, max.exists(ids.size >= _).toString)
          def item(set: dom.Node, add: dom.Node, i: Int): Unit ! W = for {
            remove <- button("button", Role.Back.classes, "data-remove" -> i.toString)(List(txt(words.remove)))
            it <- el("div", cls("okay-item"), "data-item" -> i.toString)(List(of(path :+ i.toString, of0), node(remove)))
            _ <- listen(remove, "click") { _ =>
              ids = ids.filterNot(_ == i)
              world(page(Page.Remove[Unit](it, u => u)).flatMap(_ => limited(add)))
            }
            _ <- page(Page.Insert[Unit](set, it, Some(add), u => u))
          } yield ()
          // the number taken before anything else: an item's key never one already there
          def added(set: dom.Node, add: dom.Node): Unit ! W = later {
            val i = next
            next += 1; ids = ids :+ i
            item(set, add, i).flatMap(_ => limited(add))
          }
          for {
            legend <- el("legend", cls(Role.Label.classes))(List(txt(legendText)))
            add <- button("button", Role.Action.classes, "data-add" -> id(path))(List(txt(words.add)))
            set <- el("fieldset", cls(Role.Field.classes))(List(node(legend), node(add)))
            _ <- seq((0 until (min max 0)).toList.map(_ => added(set, add)))
            _ <- limited(add)
            _ <- listen(add, "click")(_ => world(added(set, add)))
            _ <- later { items(path) = () => ids; unit }
          } yield set
        case Schema.Many(_, _, _) =>
          el("textarea", "id" -> id(path), "placeholder" -> words.onePerLine)(Nil).flatMap { area =>
            later { areas(path) = area; labelled(path, field, area) }
          }
        case leaf =>
          val typ = leaf match {
            case Schema.Integer(_) | Schema.Number(_) => "number"
            case Schema.Bool => "checkbox"
            case Schema.Date => "date"
            case _ => if (field.exists(_.secret)) "password" else "text"
          }
          // A FRACTION IS A NUMBER: without a step a browser takes 1 and refuses 12.5 before the form is sent
          val step = if (leaf match { case Schema.Number(_) => true; case _ => false }) Seq("step" -> "any") else Nil
          for {
            in <- el("input", (Seq("id" -> id(path), "type" -> typ) ++ step): _*)(Nil)
            _ <- field.flatMap(_.default).fold(unit)(d =>
              put(in, Page.Value, d match { case Json.Str(s) => s; case other => Json.print(other) }))
            _ <- later { inputs(path) = in; unit }
            l <- labelled(path, field, in)
          } yield l
      }
      private def select(path: List[String], options: Seq[String]): dom.Node ! W =
        el("select", "id" -> id(path))(options.toList.map(o => el("option", "value" -> o)(List(txt(o))))).flatMap {
          sel => later { selects(path) = sel; node(sel) }
        }
      private def labelled(path: List[String], field: Option[Schema.Field], control: dom.Node): dom.Node ! W = {
        val words: String =
          field.flatMap(_.label).orElse(field.map(_.name)).orElse(path.lastOption).getOrElse("value")
        val doc = field.flatMap(_.doc)
        doc.fold(unit)(d => put(control, Page.Attr("aria-description"), d)).flatMap(_ =>
          el("div", cls(Role.Field.classes))(List(
            el("label", cls(Role.Label.classes), "for" -> id(path))(List(txt(words))), node(control)) ++
            doc.toList.map(d => el("small", cls(Role.Doc.classes))(List(txt(d))))))
      }

      /** what was typed, as JSON along the schema; blank is absent */
      def collect(path: List[String], schema: Schema): Json ! W = schema match {
        case Schema.Record(fs) =>
          seq(fs.toList.map(f => collect(path :+ f.name, f.schema).map(f.name -> _))).map(vs =>
            Json.Obj(vs.filterNot(_._2 == Json.Null).toVector))
        case Schema.Described(of0, _, _) => collect(path, of0)
        case Schema.Optional(of0) => collect(path, of0)
        case Schema.Choice(_) =>
          selects.get(path).fold(pure[Json, W](Json.Null))(s => get(s, Page.Value).map(v => Json.Str(v)))
        case Schema.OneOf(cases) => selects.get(path).fold(pure[Json, W](Json.Null))(s =>
            get(s, Page.Value).flatMap(n => cases.find(_._1 == n) match {
              case Some((n, cs)) => collect(path :+ n, cs).map {
                  case o: Json.Obj if Schema.isRecord(cs) => ("type" -> Json.Str(n)) +: o
                  case v => Json.Obj.of("type" -> Json.Str(n), "value" -> v)
                }
              case None => pure[Json, W](Json.Null)
            }))
        case Schema.Many(of0, _, _) if items.contains(path) =>
          seq(items(path)().toList.map(i => collect(path :+ i.toString, of0))).map(vs => Json.Arr(vs.toVector))
        case Schema.Many(of0, _, _) => areas.get(path).fold(pure[Json, W](Json.Null))(a => get(a, Page.Value).map(v =>
            Json.Arr(v.split("\n").toVector.map(_.trim).filter(_.nonEmpty).map(l => typed(of0, l)))))
        case Schema.Bool =>
          inputs.get(path).fold(pure[Json, W](Json.Null))(i => get(i, Page.Checked).map(c => Json.Bool(c == "true")))
        case leaf => inputs.get(path).fold(pure[Json, W](Json.Null))(i => get(i, Page.Value).map(v =>
            if (v.isEmpty) Json.Null else typed(leaf, v)))
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

  // ---- A VIEW: semantic tags, classes for a stylesheet, nothing else — the Laminar face's, tag for tag

  /** a `View` drawn; `ref`: what a `Ref` does when the step offers the action `ref` — a click chooses it with the key */
  def view(v: View, ref: Option[String => Unit]): dom.Node ! W = v match {
    case View.Empty => el("span")(Nil)
    case View.Text(s) => el("p", cls(Role.Text.classes))(List(txt(s)))
    case View.Heading(1, s) => el("h1", cls(Role.Heading(1).classes))(List(txt(s)))
    case View.Heading(2, s) => el("h2", cls(Role.Heading(2).classes))(List(txt(s)))
    case View.Heading(l, s) => el("h3", cls(Role.Heading(l).classes))(List(txt(s)))
    case View.Notice(level, s) => el("p", cls(Role.Notice(level).classes))(List(txt(s)))
    case View.Fields(rows) => el("dl", cls(Role.Fields.classes))(rows.toList.flatMap { case (k, x) =>
        List(el("dt", cls(Role.Label.classes))(List(txt(k))), el("dd")(List(view(x, ref))))
      })
    case View.Table(columns, rows) =>
      val body = el("tbody")(rows.toList.map(r => el("tr")(r.toList.map(c => el("td")(List(view(c, ref)))))))
      el("table", cls(Role.Table.classes))(
        (if (columns.forall(_.isEmpty)) Nil
         else List(el("thead")(List(el("tr")(columns.toList.map(c => el("th")(List(txt(c))))))))) :+ body)
    case View.Items(items) => el("ul", cls(Role.Items.classes))(items.toList.map(i => el("li")(List(view(i, ref)))))
    case View.Group(title, children) => el("section", cls(Role.Group.classes))(
        title.toList.map(t => el("h4", cls(Role.GroupTitle.classes))(List(txt(t)))) ++
          children.toList.map(view(_, ref)))
    case View.Ref(key, label) =>
      el("a", cls(Role.Ref.classes), "href" -> s"#$key")(List(txt(label))).flatMap { a =>
        ref.fold(node(a))(r => listen(a, "click") { e => e.preventDefault(); r(key) }.map(_ => a))
      }
    // a page ELSEWHERE (another site): a new tab, and the page opened is told nothing of this one; a page of this
    // site (a relative address, `./`): opened in place, as a link within a site is
    case View.Link(label, url) =>
      if (url.contains("://"))
        el("a", cls(Role.Link.classes), "href" -> url, "target" -> "_blank", "rel" -> "noopener noreferrer")(
          List(txt(label)))
      else el("a", cls(Role.Link.classes), "href" -> url)(List(txt(label)))
    case View.Datum(json, _) => el("pre", cls(Role.Datum.classes))(List(txt(Json.pretty(json))))
    case View.Masked(hint) => el("span", cls(Role.Masked.classes))(List(txt(hint)))
    case View.Marked(mark, x) => el("span", cls(Role.Mark(mark).classes))(List(view(x, ref)))
  }
  /** a `View` as a node, drawn now */
  def drawn(v: View): dom.Node = world(view(v, None))
}

object DomFace {
  /** THE FACE'S SETTINGS: the Laminar face's — how it looks (a `<style>` and the face's words), whether a step is an
   * entry of the browser's history (its back button then goes back a step) — and whether the face moves the keyboard
   * (a new step's first field, a problem's field) */
  final case class Settings(look: Option[Look] = None, browserHistory: Boolean = false, focus: Boolean = true)
  /** the controls a person types into */
  val fields: String = "input, select, textarea"
  /** over the implementation in scope */
  def apply[F <: Runner with Singleton]()(implicit fx: F): DomFace[F] = new DomFace[F]()(fx)
  /** THE HANDLER, whole, in the page: `DomFace.mount(Noughts.games)` */
  def mount[F <: Runner with Singleton, A](p: Program[A, okay.faces.Face], settings: Settings = Settings(),
    done: A => Unit = (_: A) => ())(implicit fx: F): Unit =
    new DomFace[F]()(fx).mount[A](p, settings, done)
}
