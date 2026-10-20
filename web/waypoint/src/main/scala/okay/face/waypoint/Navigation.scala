package okay.face.waypoint

import com.raquo.airstream.ownership.Owner
import com.raquo.waypoint.Router
import okay.effects.Runner
import okay.effects.async.Async
import okay.face.airstream.AirStreams
import okay.effects.stream.Streams

/**
 * ROUTES AS AN EFFECT: a program `Push`es a page, `Replace`s it, asks the `Current` one — and knows no router. The
 * handler is Waypoint's `Router` (`router(r)`): `pushState`, `replaceState`, `currentPageSignal.now()`; or a recording
 * one for a test (`recording(first)`): the pages asked for, with no browser history. `pages(r)` is the current page as
 * a stream, the airstream bridge's.
 */
sealed trait Navigation[P, X]
object Navigation {
  final case class Push[P, X](page: P, answer: Unit => X) extends Navigation[P, X]
  final case class Replace[P, X](page: P, answer: Unit => X) extends Navigation[P, X]
  final case class Current[P, X](answer: P => X) extends Navigation[P, X]
}

class Navigating[I <: Runner, S <: Streams[I]](val bridge: AirStreams[I, S]) {
  val fx: bridge.fx.type = bridge.fx
  import fx._

  def push[P, R[_]](page: P)(implicit in: In[({ type L[x] = Navigation[P, x] })#L, R]): Unit ! R =
    perform[({ type L[x] = Navigation[P, x] })#L].apply[R, Unit](Navigation.Push[P, Unit](page, u => u))
  def replace[P, R[_]](page: P)(implicit in: In[({ type L[x] = Navigation[P, x] })#L, R]): Unit ! R =
    perform[({ type L[x] = Navigation[P, x] })#L].apply[R, Unit](Navigation.Replace[P, Unit](page, u => u))
  def current[P, R[_]](implicit in: In[({ type L[x] = Navigation[P, x] })#L, R]): P ! R =
    perform[({ type L[x] = Navigation[P, x] })#L].apply[R, P](Navigation.Current[P, P](p => p))

  /** Waypoint's router as the handler, in place */
  def router[P](r: Router[P]): RouterHandler[P] = new RouterHandler[P](r)
  final class RouterHandler[P](r: Router[P]) {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[({ type L[x] = Navigation[P, x] })#L, R, Rest]): A ! Rest =
      handle[({ type L[x] = Navigation[P, x] })#L, R, Rest, A, A](p, new TailHandler[({ type L[x] = Navigation[P, x] })#L, Rest, A, A] {
        def ret(a: A) = pure(a)
        def answer[X](op: Navigation[P, X]): X = op match {
          case Navigation.Push(page, answer) => r.pushState(page); answer(())
          case Navigation.Replace(page, answer) => r.replaceState(page); answer(())
          case Navigation.Current(answer) => answer(r.currentPageSignal.now())
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
  /** the current page as a stream: now, and each change */
  def pages[P](r: Router[P])(implicit owner: Owner): bridge.streams.Stream[P, Async] = bridge.fromSignal(r.currentPageSignal)
}

object Navigating {
  def apply[I <: Runner, S <: Streams[I]](bridge: AirStreams[I, S]): Navigating[I, S] = new Navigating[I, S](bridge)
}
