package okay.faces

import okay.effects.Effects

/**
 * ONE COUNTERPART'S SESSION with a step machine: the step it is at, the history for `back`, what was told on the way —
 * what every face that serves many counterparts keeps per session (MCP, REST, HTML). The rest of a step is a program of
 * the row `R` the face runs in, so a handler inside the program may use the face's own effects (a consent asked through
 * the client). Mutable, as a session is; one per counterpart. The faces call it a dialogue (`faces.dialogue`,
 * `faces.Dialogue[A, R]`): a session is one counterpart's dialogue.
 */
object Session {
  /** what a face shows when the dialogue is over: that it is — the program's value is the program's, not shown */
  val over: View = View.info("done")
}
abstract class Session[F <: Effects with Singleton, A, R[_]](val fx: F, val allowsBack: Boolean) {
  // `allowsBack`: whether the dialogue offers going back at all; `fx` the implementation, so that any face over it
  // takes a session made anywhere over it — the type is `Session[Machine.type, A, R]`, no path in it. Made by
  // `Faces.dialogue`, which gives `first`
  import fx._
  type M[X] = X ! R
  protected def first: Step[A, M] ! R
  private var told: Vector[View] = Vector.empty
  private var history: List[Step.Waiting[A, M]] = Nil
  private var step: Option[Step[A, M]] = None
  /** an event fired while a step waited: its rest, run at the next `current` — a session hears when next asked */
  @volatile private var fired: Option[(Step.Waiting[A, M], M[Step[A, M]])] = None
  private def settle(s: Step[A, M]): Step[A, M] ! R = s match {
    case Step.Told(view, next) => told = told :+ view; next.flatMap(settle)
    case w: Step.Waiting[A, M] @unchecked =>
      w.listen(next => if (step.contains(w) || step.isEmpty) fired = Some((w, next))); pure[Step[A, M], R](w)
    case other => pure[Step[A, M], R](other)
  }
  /** the step, the first one run when first asked; an event that fired meanwhile moved it on */
  def current: Step[A, M] ! R = fired match {
    case Some((w, next)) if step.contains(w) =>
      fired = None; told = Vector.empty
      next.flatMap(settle).map { s => history = if (w.back) w :: history else history; step = Some(s); s }
    case _ => step match {
        case Some(s) => pure[Step[A, M], R](s)
        case None => first.flatMap(settle).map { s => step = Some(s); s }
      }
  }
  /** whether the step waits on an event too: a client may ask again later */
  def listening: Boolean ! R =
    current.map { case w: Step.Waiting[A, M] @unchecked => w.meanwhile.isDefined; case _ => false }
  /** what was told since the last step, and the step's own view, as one stack */
  def view: View ! R = shown.map { case (ts, v) => View.Group(None, ts :+ v) }
  /** the same apart: what was told, and the step's own view — for a face that shows them differently */
  def shown: (Vector[View], View) ! R = current.map {
    case w: Step.Waiting[A, M] @unchecked => (told, w.view)
    case Step.Done(_) => (told, Session.over)
    case Step.Told(v, _) => (Vector.empty, v)
  }
  def actions: List[Action[_]] ! R = current.map { case w: Step.Waiting[A, M] @unchecked => w.actions; case _ => Nil }
  def done: Boolean ! R = current.map { case Step.Done(_) => true; case _ => false }
  def canGoBack: Boolean = allowsBack && history.nonEmpty
  /** the choice taken: the view after, or the problems, the step unchanged */
  def choose(chosen: Chosen): Either[Rejected, View] ! R = current.flatMap {
    case w: Step.Waiting[A, M] @unchecked => w.resume(chosen) match {
        case Left(r) => pure[Either[Rejected, View], R](Left(r))
        case Right(next) => {
          told = Vector.empty;
          next.flatMap(settle).flatMap { s =>
            history = if (w.back) w :: history else history; step = Some(s); view.map(Right(_))
          }
        }
      }
    case _ =>
      pure[Either[Rejected, View], R](Left(Rejected(List(okay.effects.shape.Problem(Nil, "the dialogue is over")))))
  }
  def back(): Boolean = history match {
    case previous :: older if canGoBack => told = Vector.empty; step = Some(previous); history = older; true
    case _ => false
  }
}
