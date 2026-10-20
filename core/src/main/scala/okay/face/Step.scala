package okay.face

import okay.effects.shape.Problem

/**
 * THE STEP MACHINE: a program of `Face` turned into steps by its standard handler, which draws nothing. Every face is a
 * loop over `Step`: show the view with its actions, read a `Chosen`, `resume` — and RUN the rest, `M[Step]`, a program
 * of the row that remains (`[X] => X ! Rest`): by `run` when nothing remains, to a `Future` for a browser, by the
 * world's handlers for a console. What a face adds is those things and no more. `resume` holds the program's
 * continuation, deep and resumable any number of times: a face that keeps earlier `Waiting`s has BACK for free —
 * whatever handlers were applied INSIDE `Face` have their state in the continuation and come back with it; the world's
 * handlers outside do not.
 */
sealed trait Step[A, M[_]]
object Step {
  final case class Done[A, M[_]](a: A) extends Step[A, M]
  /** shown, and waiting: `resume` decodes the choice against `actions` — `Left(Rejected)`, and the face shows the problems
   * and waits on THIS step again — or the rest, a program of the remaining row */
  final case class Waiting[A, M[_]](view: View, actions: List[Action[_]], resume: Chosen => Either[Rejected, M[Step[A, M]]],
                                    meanwhile: Option[Meanwhile[M[Step[A, M]]]] = None) extends Step[A, M] {
    /** a face that listens: the rest of the program when the event fires, to run as `resume`'s */
    def listen(k: M[Step[A, M]] => Unit): () => Unit = meanwhile.fold(() => ())(_.register(k))
  }
  /** told, and going on at once: a face shows it and runs the rest */
  final case class Told[A, M[_]](view: View, next: M[Step[A, M]]) extends Step[A, M]

  /** the action named, taken with the arguments, or why not */
  def take[A](chosen: Chosen, actions: List[Action[A]]): Either[Rejected, A] = actions.find(_.name == chosen.action) match {
    case None => Left(Rejected(List(Problem(List("action"), s"one of ${actions.map(_.name).mkString(", ")}"))))
    case Some(a) => a.taken(chosen.args).left.map(Rejected(_))
  }
}
