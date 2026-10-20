package okay.face

import okay.effects.Effects

/**
 * ONE COUNTERPART'S SESSION with a step machine: the step it is at, the history for `back`, what was told on the way —
 * what every face that serves many counterparts keeps per session (MCP, REST, HTML). The rest of a step is a program of
 * the row `R` the face runs in, so a handler inside the program may use the face's own effects (a consent asked through
 * the client). Mutable, as a session is; one per counterpart.
 */
class Dialogues[F <: Effects](implicit implementation: F) {
  val fx: F = implementation
  import fx._

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
    /** what was told since the last step, and the step's own view */
    def view: View ! R = current.map {
      case w: Step.Waiting[A, M] @unchecked => View.Group(None, told :+ w.view)
      case Step.Done(a) => View.Group(None, told :+ View.text(s"done: $a"))
      case Step.Told(v, _) => v
    }
    def actions: List[Action[_]] ! R = current.map { case w: Step.Waiting[A, M] @unchecked => w.actions; case _ => Nil }
    def done: Boolean ! R = current.map { case Step.Done(_) => true; case _ => false }
    def canGoBack: Boolean = history.nonEmpty
    /** the choice taken: the view after, or the problems, the step unchanged */
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
  def dialogue[A, R[_]](first: Step[A, ({ type L[X] = X ! R })#L] ! R): Dialogue[A, R] = new Dialogue[A, R](first)
}
