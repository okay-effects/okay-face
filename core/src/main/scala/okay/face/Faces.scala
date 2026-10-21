package okay.face

import okay.effects.Effects
import okay.effects.shape.Shape

/**
 * THE WORDS of `Face`, over any implementation `fx`, and the stepping handler. A scenario is a program in
 * `Face + Domain`; a component is a function returning one, with a result — `ShowModal`.
 */
class Faces[F <: Effects](implicit implementation: F) {
  val fx: F = implementation
  import fx._
  /** sessions with a step machine, for the faces that serve many counterparts */
  val dialogues: Dialogues[fx.type] = new Dialogues[fx.type]()(fx)
  type Dialogue[A, R[_]] = dialogues.Dialogue[A, R]
  def dialogue[A, R[_]](first: Step[A, ({ type L[X] = X ! R })#L] ! R, back: Boolean = true): Dialogue[A, R] = dialogues.dialogue[A, R](first, back)

  def offer[A, R[_]](view: View, actions: List[Action[A]])(implicit in: In[Face, R]): A ! R =
    perform[Face].apply[R, A](Face.Offer[A, A](view, actions, a => a))
  /** show, and wait for an action or for the event, whichever comes first */
  def offer[A, R[_]](view: View, actions: List[Action[A]], meanwhile: Meanwhile[A])(implicit in: In[Face, R]): A ! R =
    perform[Face].apply[R, A](Face.Offer[A, A](view, actions, a => a, Some(meanwhile)))
  /** show until something happens: no action, the event alone — a face that cannot listen is stuck here, so give an action where that matters */
  def await[A, R[_]](view: View, meanwhile: Meanwhile[A])(implicit in: In[Face, R]): A ! R = offer[A, R](view, Nil, meanwhile)
  /** show, and go on. (`show`, not `tell`: the standard Writer's word is `tell`, the Reader's `ask`, and a scenario imports both) */
  def show[R[_]](view: View)(implicit in: In[Face, R]): Unit ! R = perform[Face].apply[R, Unit](Face.Show[Unit](view, u => u))

  /** yes or no */
  def confirm[R[_]](question: View)(implicit in: In[Face, R]): Boolean ! R =
    offer[Boolean, R](question, List(Action.plain[Boolean]("yes", "Yes")(true), Action.plain[Boolean]("no", "No")(false)))
  /** one of the options, each a plain action named from its label */
  def choose[A, R[_]](view: View, options: List[(String, A)])(implicit in: In[Face, R]): A ! R = {
    val names = options.map(o => Action.nameOf(o._1))
    val unique = names.zipWithIndex.map { case (n, i) => if (names.indexOf(n) == i) n else s"$n-${i + 1}" }
    offer[A, R](view, options.zip(unique).map { case ((label, a), name) => Action.plain[A](name, label)(a) })
  }
  /** a form, filled and submitted */
  def fill[P, R[_]](view: View, form: Shape[P])(implicit in: In[Face, R]): P ! R =
    offer[P, R](view, List(Action[P, P]("submit", "Submit", form)(p => p)))
  /** a form, filled and submitted, or cancelled */
  def fillOrCancel[P, R[_]](view: View, form: Shape[P])(implicit in: In[Face, R]): Option[P] ! R =
    offer[Option[P], R](view, List(Action[P, Option[P]]("submit", "Submit", form)(p => Some(p)), Action.plain[Option[P]]("cancel", "Cancel")(None)))

  /** THE STEPPING HANDLER: `Face` taken out of the row, the program a `Step` of what remains. `Offer` becomes a
   * `Waiting` whose `resume` decodes the choice and gives the rest as a program; `Show` a `Told`. General: `k` is kept */
  def steps[A, R[_], Rest[_]](p: A ! R)(implicit sp: Split[Face, R, Rest]): Step[A, ({ type L[X] = X ! Rest })#L] ! Rest = {
    type M[X] = X ! Rest
    handle[Face, R, Rest, A, Step[A, M]](p, new Handler[Face, Rest, A, Step[A, M]] {
      def ret(a: A) = pure(Step.Done[A, M](a))
      def op[X](op: Face[X], k: X => Step[A, M] ! Rest) = op match {
        case Face.Offer(view, actions, answer, meanwhile) =>
          pure[Step[A, M], Rest](Step.Waiting[A, M](view, actions, chosen => Step.take(chosen, actions).map(a => k(answer(a))), meanwhile.map(_.map(a => k(answer(a))))))
        case Face.Show(view, answer) => pure[Step[A, M], Rest](Step.Told[A, M](view, k(answer(()))))
      }
    })
  }
}

object Faces {
  def apply()(implicit fx: Effects): Faces[fx.type] = {
    implicit val exact: fx.type = fx
    new Faces[fx.type]
  }
}
