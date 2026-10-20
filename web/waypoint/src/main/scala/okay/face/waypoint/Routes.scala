package okay.face.waypoint

import com.raquo.airstream.ownership.Owner
import com.raquo.waypoint.Router
import okay.effects.Runner
import okay.effects.async.Async
import okay.effects.stream.Streams
import okay.face.Navigation
import okay.face.airstream.AirStreams

/** THE BROWSER'S HANDLER OF `Navigation` (okay-face core's effect): Waypoint's `Router` — `pushState`, `replaceState`,
 * `currentPageSignal.now()`; and the current page as a stream, the airstream bridge's */
class Routes[I <: Runner, S <: Streams[I]](val bridge: AirStreams[I, S]) {
  val fx: bridge.fx.type = bridge.fx
  import fx._

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
  /** the current page as a stream: now, and each change */
  def pages[P](r: Router[P])(implicit owner: Owner): bridge.streams.Stream[P, Async] = bridge.fromSignal(r.currentPageSignal)
}
