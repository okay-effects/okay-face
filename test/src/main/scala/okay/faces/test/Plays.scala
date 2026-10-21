package okay.faces.test

import okay.effects.Runner
import okay.effects.tagless.Program
import okay.faces.{Face, Chosen, Faces, Rejected, Step, View}

/**
 * THE TEST FACE: a step machine driven by a SCRIPT of choices, every view shown collected, BACK as a script entry — the
 * face's history of `Waiting`s re-resumed. Over a `Runner` whose row is done: the rest of each step is `run`.
 * The law it states: two faces given the same script show the same views.
 */
class Plays[F <: Runner with Singleton](implicit implementation: F) {
  val fx: F = implementation
  import fx._
  type M[X] = X ! okay.effects.Void

  sealed trait Entry
  final case class Pick(chosen: Chosen) extends Entry
  /** one step back: the previous `Waiting` is waited on again */
  case object Back extends Entry
  /** the step's event fired — `trigger` fires it (a `Meanwhile.manual`'s `fire`) — and the rest run, as a face that listens would */
  final case class Fire(trigger: () => Unit) extends Entry
  final case class Script(entries: List[Entry])
  object Script {
    def apply(entries: Entry*): Script = new Script(entries.toList)
    def of(choices: Chosen*): Script = new Script(choices.toList.map(Pick(_)))
  }

  sealed trait Played[A] { def views: List[View] }
  object Played {
    final case class Finished[A](views: List[View], a: A) extends Played[A]
    /** the script ended at a `Waiting`, or a choice was rejected there */
    final case class Stuck[A](views: List[View], at: Step.Waiting[A, M], rejected: Option[Rejected]) extends Played[A]
  }

  /** THE HANDLER: a program of the face played by a script — what the test face shows and how it ends */
  def handle[A](p: Program[A, okay.faces.Face])(script: Script): Played[A] =
    // the faces' words are over `faces.fx`, this face's over `fx`: one value, two paths the checker cannot join
    play[A](faces.steps[A, okay.faces.Face, faces.fx.Pure](faces.of(p))(faces.fx.splitOne[okay.faces.Face]).asInstanceOf[fx.![Step[A, M], okay.effects.Void]], script)
  val faces: Faces[F] = new Faces[F]()(fx)
  def play[A](first: Step[A, M] ! okay.effects.Void, script: Script): Played[A] = {
    // `Told`s run through; `history` the Waitings passed, latest first, for Back
    def settle(step: Step[A, M], views: List[View]): (Step[A, M], List[View]) = step match {
      case Step.Told(view, next) => settle(run(next), view :: views)
      case other => (other, views)
    }
    def loop(step: Step[A, M], views: List[View], history: List[Step.Waiting[A, M]], entries: List[Entry]): Played[A] = step match {
      case Step.Done(a) => Played.Finished(views.reverse, a)
      case w: Step.Waiting[A, M] @unchecked =>
        val shown = w.view :: views
        var pending: Option[M[Step[A, M]]] = None
        val cancel = w.listen(next => pending = Some(next))
        entries match {
          case Nil => cancel(); Played.Stuck(shown.reverse, w, None)
          case Fire(trigger) :: rest =>
            trigger(); cancel()
            pending match {
              case Some(next) => val (settled, more) = settle(run(next), shown); loop(settled, more, if (w.back) w :: history else history, rest)
              case None => Played.Stuck(shown.reverse, w, None)
            }
          case Back :: rest => cancel(); history match {
            case previous :: older => loop(previous, shown, older, rest)
            case Nil => loop(w, shown, Nil, rest)                      // nothing to go back to: the same step again
          }
          case Pick(c) :: rest => cancel(); w.resume(c) match {
            case Left(rejected) => Played.Stuck(shown.reverse, w, Some(rejected))
            case Right(next) =>
              val (settled, more) = settle(run(next), shown)
              loop(settled, more, if (w.back) w :: history else history, rest)
          }
        }
      case Step.Told(_, _) =>
        val (settled, more) = settle(step, views); loop(settled, more, history, entries)
    }
    val (settled, views) = settle(run(first), Nil)
    loop(settled, views, Nil, script.entries)
  }
}

object Plays {
  def apply()(implicit fx: Runner): Plays[fx.type] = {
    implicit val exact: fx.type = fx
    new Plays[fx.type]
  }
}
