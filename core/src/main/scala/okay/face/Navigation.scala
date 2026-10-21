package okay.face

import okay.effects.Effects

/**
 * ROUTES AS AN EFFECT, of no platform: a program `push`es a page, `replace`s it, asks the `current` one — a page any
 * value the application names its places by — and knows no router, no window, no URL. Where it lands is the face's
 * handler: a browser's history through Waypoint (`web/waypoint`'s `router`), a window's title, a terminal's line, a
 * stack in memory (`memory`) that tells the face of each change, a recording for a test (`recording`).
 */
sealed trait Navigation[P, X]
object Navigation {
  final case class Push[P, X](page: P, answer: Unit => X) extends Navigation[P, X]
  final case class Replace[P, X](page: P, answer: Unit => X) extends Navigation[P, X]
  final case class Current[P, X](answer: P => X) extends Navigation[P, X]
}

/** the pages in memory: where the program is, where it was; a face reads `current`, is told of changes, may go `back` */
final class Pages[P](first: P) {
  private var stack: List[P] = List(first)
  private var listeners: List[P => Unit] = Nil
  def current: P = stack.head
  def history: List[P] = stack
  def push(p: P): Unit = { stack = p :: stack; tell() }
  def replace(p: P): Unit = { stack = p :: stack.drop(1); tell() }
  /** one page back, where there is one */
  def back(): Boolean = stack match { case _ :: rest if rest.nonEmpty => stack = rest; tell(); true; case _ => false }
  def onChange(f: P => Unit): Unit = listeners = f :: listeners
  private def tell(): Unit = listeners.foreach(_(current))
}

class Navigations[F <: Effects with Singleton](implicit implementation: F) {
  val fx: F = implementation
  import fx._

  def push[P, R[_]](page: P)(implicit in: In[({ type L[x] = Navigation[P, x] })#L, R]): Unit ! R =
    perform[({ type L[x] = Navigation[P, x] })#L].apply[R, Unit](Navigation.Push[P, Unit](page, u => u))
  def replace[P, R[_]](page: P)(implicit in: In[({ type L[x] = Navigation[P, x] })#L, R]): Unit ! R =
    perform[({ type L[x] = Navigation[P, x] })#L].apply[R, Unit](Navigation.Replace[P, Unit](page, u => u))
  def current[P, R[_]](implicit in: In[({ type L[x] = Navigation[P, x] })#L, R]): P ! R =
    perform[({ type L[x] = Navigation[P, x] })#L].apply[R, P](Navigation.Current[P, P](p => p))

  /** the pages kept in memory, in place; the face given the `Pages` reads and listens */
  def memory[P](pages: Pages[P]): Memory[P] = new Memory[P](pages)
  final class Memory[P](pages: Pages[P]) {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[({ type L[x] = Navigation[P, x] })#L, R, Rest]): A ! Rest =
      handle[({ type L[x] = Navigation[P, x] })#L, R, Rest, A, A](p, new TailHandler[({ type L[x] = Navigation[P, x] })#L, Rest, A, A] {
        def ret(a: A) = pure(a)
        def answer[X](op: Navigation[P, X]): X = op match {
          case Navigation.Push(page, answer) => pages.push(page); answer(())
          case Navigation.Replace(page, answer) => pages.replace(page); answer(())
          case Navigation.Current(answer) => answer(pages.current)
        }
      })
  }
  /** the pages asked for, in order, and the value — a test's; `Replace` replaces the last */
  def recording[P](first: P): Recording[P] = new Recording[P](first)
  final class Recording[P](first: P) {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[({ type L[x] = Navigation[P, x] })#L, R, Rest]): (List[P], A) ! Rest =
      handle[({ type L[x] = Navigation[P, x] })#L, List[P], R, Rest, A, (List[P], A)](p, List(first),
        new StatefulTailHandler[({ type L[x] = Navigation[P, x] })#L, List[P], Rest, A, (List[P], A)] {
          def ret(s: List[P], a: A) = pure((s.reverse, a))
          def answer[X](cell: Cell[List[P]], op: Navigation[P, X]): X = op match {
            case Navigation.Push(page, answer) => cell.set(page :: cell.get); answer(())
            case Navigation.Replace(page, answer) => cell.set(page :: cell.get.drop(1)); answer(())
            case Navigation.Current(answer) => answer(cell.get.head)
          }
        })
  }
}

object Navigations {
  def apply()(implicit fx: Effects): Navigations[fx.type] = {
    implicit val exact: fx.type = fx
    new Navigations[fx.type]
  }
}
