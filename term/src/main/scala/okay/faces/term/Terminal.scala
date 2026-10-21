package okay.faces.term

import okay.effects.json.Json
import okay.effects.shape.{Fields, Problem, Problems, Reading, Schema}
import okay.effects.tagless.{Program, Tagless}
import okay.effects.world.{Console, Io}
import okay.faces.{Action, Chosen, Face, Faces, Paint, Rejected, Role, Step, View, Words}

/**
 * THE TERMINAL FACE: a line dialogue — a program in okay-effects-io's `Console`, so it runs under `realConsole`, and
 * under `console(lines)` in a test. Two things, as every face: a `Step` loop (`terminal`) that shows a view and its
 * actions as a numbered menu and reads a choice; and the PERSON as a handler of `Fields` (`prompting`): an action's
 * parameters asked field by field, the label shown, a problem shown and the field asked again. `<` is back, `?` the
 * actions' docs, `q` quits. Plain text unless a `Paint` is given: `Ansi(look)` colours by role for a terminal, and a
 * pipe gets none.
 */
class Terminal[F <: okay.effects.Effects with Singleton](paint: Paint = Paint.none, width: Int = 80,
  words: Words = Words.english)(implicit val fx: F) {
  import fx._
  val faces: Faces[fx.type] = new Faces[fx.type]()(fx)
  val io: Io[fx.type] = new Io[fx.type]()(fx)
  val reading: Reading[fx.type] = new Reading[fx.type]()(fx)
  import io.{printLine, readLine, readSecret}

  private def say[R[_]](lines: List[String])(implicit c: In[Console, R]): Unit ! R =
    lines.foldLeft(pure[Unit, R](()))((acc, l) => acc.flatMap(_ => printLine[R](l)))
  private def problems[R[_]](ps: Problems)(implicit c: In[Console, R]): Unit ! R =
    say[R](ps.map(p => paint(Role.Problems, s"${words.problem}: " + p.render)))

  // ---- THE PERSON AS A HANDLER OF Fields
  /** a field's words: its label, else its name, else what it is */
  private def labelOf(path: List[String], f: Schema.Field): String =
    f.label.orElse(Option(f.name).filter(_.nonEmpty)).orElse(path.lastOption).getOrElse("value")
  private def hintOf(schema: Schema): Option[String] = schema match {
    case Schema.Integer(_) => Some("a whole number")
    case Schema.Number(_) => Some("a number")
    case Schema.Bool => Some("y/n")
    case Schema.Date => Some("yyyy-mm-dd")
    case Schema.Described(of, _, _) => hintOf(of)
    case _ => None
  }
  private def stripped(schema: Schema): Schema =
    schema match { case Schema.Described(of, _, _) => stripped(of); case other => other }
  /** a typed line as the JSON the field's shape decodes: numbers and booleans as such, the rest text */
  private def typed(schema: Schema, line: String): Json = stripped(schema) match {
    case Schema.Integer(_) | Schema.Number(_) =>
      scala.util.Try(BigDecimal(line.trim)).toOption.fold[Json](Json.Str(line))(Json.Num(_))
    case Schema.Bool => line.trim.toLowerCase match {
        case "y" | "yes" | "true" => Json.Bool(true)
        case "n" | "no" | "false" => Json.Bool(false)
        case _ => Json.Str(line)
      }
    case _ => Json.Str(line)
  }
  private def menu[R[_]](options: List[(String, String)])(implicit c: In[Console, R]): Option[String] ! R = {
    val lines = options.zipWithIndex.map { case ((_, label), i) => s"${i + 1}) $label" }
    // the NAME first, then the number: a choice's names may be digits themselves
    say[R](lines).flatMap(_ => printLine[R]("> ")).flatMap(_ => readLine[R]).map(_.flatMap { l =>
      val t = l.trim
      options.collectFirst { case (n, _) if n == t => n }.orElse(
        t.toIntOption.filter(i => i >= 1 && i <= options.size).map(i => options(i - 1)._1))
    })
  }
  /** one leaf asked: the label, the hint and the default; an empty line is nothing entered. An OPTIONAL leaf is asked
   * as the leaf, with "empty for none" in the hint */
  private def prompt[R[_]](path: List[String], f: Schema.Field)(implicit c: In[Console, R]): Option[Json] ! R =
    (stripped(f.schema) match { case Schema.Optional(of) => stripped(of); case s => s }) match {
      case Schema.Choice(options) =>
        printLine[R](labelOf(path, f) + ":").flatMap(_ => menu[R](options.map(o => (o, o)))).map(_.map(Json.Str(_)))
      case other =>
        val optional = stripped(f.schema) match { case Schema.Optional(_) => List(words.optional); case _ => Nil }
        val hint = (hintOf(other).toList ++ f.default.map(d =>
          s"${words.default} " + Json.print(d)).toList ++ f.doc.toList ++ optional ++
          (if (f.secret) List(words.hidden) else Nil)).mkString(", ")
        val label = labelOf(path, f) + (if (hint.isEmpty) "" else s" ($hint)") + ": "
        // a secret is typed without echo, where the console can hide it
        printLine[R](label).flatMap(_ => if (f.secret) readSecret[R] else readLine[R]).map {
          case None => None
          case Some(l) if l.trim.isEmpty => None
          case Some(l) => Some(typed(other, l))
        }
    }

  def prompting[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[Fields, R, Rest], c: In[Console, Rest]): A ! Rest = {
    // an optional leaf is asked when its presence is: what was typed then answers the question for its value
    val entered = scala.collection.mutable.Map.empty[List[String], Json]
    fx.handle[Fields, R, Rest, A, A](p,
      new Handler[Fields, Rest, A, A] {
        def ret(a: A) = pure(a)
        def op[X](op: Fields[X], k: X => A ! Rest) = op match {
          case Fields.Get(path, f, answer) => entered.remove(path) match {
              case Some(j) => k(answer(Some(j)))
              case None => prompt[Rest](path, f).flatMap(j => k(answer(j)))
            }
          case Fields.Retry(path, f, ps, answer) =>
            problems[Rest](ps).flatMap(_ => prompt[Rest](path, f)).flatMap(j => k(answer(j)))
          case Fields.Has(path, f, answer) => stripped(f.schema) match {
              // a person is asked whether there is something only where it is OPTIONAL; a record, a sum or a list that
              // is
              // asked for is there — its fields, its case, its size are the questions
              case Schema.Optional(of) if okay.faces.FormJson.leafish(of) =>
                // a leaf that may be absent: asked at once, an empty line is none
                prompt[Rest](path, f).flatMap {
                  case None => k(answer(Fields.Presence.Absent))
                  case Some(j) => entered(path) = j; k(answer(Fields.Presence.Present))
                }
              case Schema.Optional(_) =>
                printLine[Rest](s"${labelOf(path, f)}? (y/n): ").flatMap(_ => readLine[Rest]).flatMap { l =>
                  val yes = l.exists(x => Set("y", "yes", "true").contains(x.trim.toLowerCase))
                  k(answer(if (yes) Fields.Presence.Present else Fields.Presence.Absent))
                }
              case _ => k(answer(Fields.Presence.Present))
            }
          case Fields.Size(path, f, answer) =>
            printLine[Rest](s"${words.howMany} ${labelOf(path, f)}? ").flatMap(_ => readLine[Rest]).flatMap(l =>
              k(answer(l.flatMap(_.trim.toIntOption).filter(_ >= 0))))
          case Fields.Which(path, cases, answer) =>
            printLine[Rest](s"${path.lastOption.getOrElse("which")}:").flatMap(_ =>
              menu[Rest](cases.map(x => (x, x)))).flatMap(w => k(answer(w)))
        }
      })
  }

  /** THE HANDLER: a program of the face at a terminal, `Console` left in the row for a console handler — a scripted one
   * in a
   * test, the real one by `console`; `None` when the person quits */
  def handle[A](p: Program[A, okay.faces.Face], back: Boolean = true): Option[A] ! C =
    terminal[A, C](faces.steps[A, CF, C](faces.of(p).into[CF]), back)
  /** the same at the real console: `run(Terminal().console(Noughts.games))` */
  def console[A](p: Program[A, okay.faces.Face], back: Boolean = true): Option[A] ! Pure =
    io.realConsole[C, Pure, Option[A]](handle[A](p, back))
  type C[X] = okay.effects.Sum[Pure, Console, X]
  type CF[X] = okay.effects.Sum[C, Face, X]

  // ---- THE STEP LOOP
  /** the face: shows, reads, resumes; `None` when the person quits. The row holds `Console` and whatever the steps need
   */
  def terminal[A, R[_]](first: Step[A, ({ type L[X] = X ! R })#L] ! R, back: Boolean = true)(
    implicit c: In[Console, R]): Option[A] ! R = {
    type M[X] = X ! R
    type RF[X] = okay.effects.Sum[R, Fields, X]
    // LIVE REFS: when the step offers `ref`, a `Ref`'s key typed is the terminal's click — the view's `[key]`s are the
    // choices, and the action itself stays out of the menu, as it does where a face clicks
    def live(w: Step.Waiting[A, M]): Boolean = w.actions.exists(_.name == "ref")
    def keys(v: View): List[String] = v match {
      case View.Ref(key, _) => List(key)
      case View.Marked(_, x) => keys(x)
      case View.Group(_, cs) => cs.toList.flatMap(keys)
      case View.Table(_, rows) => rows.toList.flatMap(_.toList.flatMap(keys))
      case View.Items(items) => items.toList.flatMap(keys)
      case View.Fields(rows) => rows.toList.flatMap { case (_, x) => keys(x) }
      case _ => Nil
    }
    def menu(w: Step.Waiting[A, M]): List[Action[_]] = if (live(w)) w.actions.filterNot(_.name == "ref") else w.actions
    def menuOf(w: Step.Waiting[A, M], canGoBack: Boolean): List[String] = {
      val entries = menu(w).zipWithIndex.map { case (a, i) => paint(Role.Action, s"${i + 1}) ${a.label}") }
      val hint = (if (live(w) && keys(w.view).nonEmpty) List(words.keyHint) else Nil) ++
        (if (canGoBack) List(words.backHint) else Nil) ++ List(words.helpHint, words.quitHint)
      List(entries.mkString("  ") + paint(Role.Doc, hint.mkString("   (", ", ", ")")))
    }
    /** the help: each action's line, wrapped at `width`, its continuation indented under the text */
    def docs(w: Step.Waiting[A, M]): List[String] =
      ((if (live(w)) w.actions.find(_.name == "ref").map(a => words.typeAKey -> a.doc.getOrElse(a.label)).toList
        else Nil) ++ menu(w).map(a => a.name -> a.doc.getOrElse(a.label)))
        .flatMap { case (head, text) => Terminal.wrap(s"$head: $text", width, head.length + 2) }
    def pick(w: Step.Waiting[A, M], line: String): Option[Action[_]] = {
      val t = line.trim
      val m = menu(w)
      m.find(_.name == t).orElse(t.toIntOption.filter(i => i >= 1 && i <= m.size).map(i => m(i - 1)))
    }
    /** the action's arguments from the person: the parameters read field by field, or nothing for a plain action */
    def arguments(a: Action[_]): Either[Problems, Json] ! R = a.params.erased match {
      case Schema.Record(fields) if fields.isEmpty => pure[Either[Problems, Json], R](Right(Json.Obj.empty))
      case _ =>
        prompting[RF, R, Either[Problems, a.P]](reading.read[a.P, RF](a.params)).map(_.map(p => a.params.encode(p)))
    }
    def loop(step: Step[A, M], history: List[Step.Waiting[A, M]]): Option[A] ! R = step match {
      case Step.Done(a) => pure[Option[A], R](Some(a))
      case Step.Told(view, next) => say[R](Render.view(view, paint) :+ "").flatMap(_ => next).flatMap(loop(_, history))
      case w: Step.Waiting[A, M] @unchecked =>
        say[R](Render.view(w.view, paint) :+ "").flatMap(_ => say[R](menuOf(w, back && history.nonEmpty))).flatMap(_ =>
          printLine[R]("> ")).flatMap(_ => readLine[R]).flatMap {
          case None => pure[Option[A], R](None)
          case Some(l) if l.trim == "q" => pure[Option[A], R](None)
          case Some(l) if l.trim == "<" =>
            history match {
              case previous :: older if back => loop(previous, older)
              case _ => loop(w, history)
            }
          case Some(l) if l.trim == "?" => say[R](docs(w)).flatMap(_ => loop(w, history))
          case Some(l) if live(w) && keys(w.view).contains(l.trim) =>
            w.resume(Chosen("ref", Json.obj("key" -> Json.Str(l.trim)))) match {
              case Left(Rejected(ps)) => problems[R](ps).flatMap(_ => loop(w, history))
              case Right(next) => next.flatMap(loop(_, if (w.back) w :: history else history))
            }
          case Some(l) => pick(w, l) match {
              case None =>
                val choices = menu(w).map(_.name) ++ (if (live(w)) keys(w.view).map("[" + _ + "]") else Nil)
                printLine[R](s"${words.oneOf} ${choices.mkString(", ")}").flatMap(_ => loop(w, history))
              case Some(a) => arguments(a).flatMap {
                  case Left(ps) => problems[R](ps).flatMap(_ => loop(w, history))
                  case Right(args) => w.resume(Chosen(a.name, args)) match {
                      case Left(Rejected(ps)) => problems[R](ps).flatMap(_ => loop(w, history))
                      case Right(next) => next.flatMap(loop(_, if (w.back) w :: history else history))
                    }
                }
            }
        }
    }
    first.flatMap(loop(_, Nil))
  }
}

object Terminal {
  /** words onto lines of at most `width` columns, lines after the first indented by `indent`; a word longer than the
   * room stands alone */
  def wrap(text: String, width: Int, indent: Int = 0): List[String] = {
    val pad = " " * indent
    text.split("\\s+").filter(_.nonEmpty).foldLeft(List.empty[String]) {
      case (Nil, word) => List(word)
      case (line :: done, word) =>
        if (line.length + 1 + word.length <= width) (line + " " + word) :: done else (pad + word) :: line :: done
    }.reverse
  }
  /** over the implementation in scope */
  def apply[F <: okay.effects.Effects with Singleton](paint: Paint = Paint.none, width: Int = 80,
    words: Words = Words.english)(implicit fx: F): Terminal[F] = new Terminal[F](paint, width, words)(fx)
  /** THE HANDLER, whole, at the real console: `run(Terminal.console(Noughts.games))` */
  def console[F <: okay.effects.Runner with Singleton, A](p: Program[A, okay.faces.Face], paint: Paint = Paint.none,
    width: Int = 80, back: Boolean = true, words: Words = Words.english)(
    implicit fx: F): Program[Option[A], Tagless.Pure] = {
    val term = new Terminal[F](paint, width, words)(fx)
    Faces.running(term.fx.run[Option[A]](term.console[A](p, back)))
  }
}
