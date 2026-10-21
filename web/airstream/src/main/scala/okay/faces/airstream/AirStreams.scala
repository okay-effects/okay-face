package okay.faces.airstream

import com.raquo.airstream.core.{EventStream, Observable, Observer, Signal}
import com.raquo.airstream.ownership.Owner
import com.raquo.airstream.state.Var
import okay.effects.Runner
import okay.effects.async.{Async, Asynchrony, Running}
import okay.effects.stream.Streams
import scala.collection.mutable
import scala.concurrent.ExecutionContext

/**
 * AIRSTREAM'S STREAMS AND OKAY'S, BOTH WAYS, and a `Var` as the handler of a `State`: an okay program runs inside a
 * Laminar page with no glue written by hand. `fromEventStream(es)(owner)`: a `Stream[O, Async]`, subscribed on its
 * first pull, the events between pulls buffered, the subscription the `Owner`'s (an element's, or a `ManualOwner`).
 * `toEventStream(s)`: an okay stream as an `EventStream`, started when Airstream starts it, cancelled when Airstream
 * stops it — the stream runs exactly while something observes it. `varState(v)(p)`: a `Var[S]` answering `State[S]` in
 * place, so a page sees a program's state as it changes. No `Face` here: a Laminar team may take this alone.
 */
class AirStreams[I <: Runner with Singleton, S <: Streams[I]](val streams: S)(implicit ec: ExecutionContext) {
  val fx: streams.fx.type = streams.fx
  import fx._
  import fx.std._
  import streams.Stream
  val asynchrony: Asynchrony[fx.type] = new Asynchrony[fx.type]()(fx, ec)
  import asynchrony.{cancellable, start}

  /** an EventStream's events as a stream: each pull the next event, those between pulls kept in order */
  def fromEventStream[O](es: EventStream[O])(implicit owner: Owner): Stream[O, Async] = fromObservable(es)
  /** a Signal's current value first, then its changes — as a Signal observed gives them */
  def fromSignal[A](signal: Signal[A])(implicit owner: Owner): Stream[A, Async] = fromObservable(signal)
  private def fromObservable[O](es: Observable[O])(implicit owner: Owner): Stream[O, Async] = {
    val buffer = mutable.Queue.empty[O]
    var waiting: Option[O => Unit] = None
    var subscribed = false
    def subscribe(): Unit = if (!subscribed) {
      subscribed = true
      es.foreach { o => waiting match { case Some(k) => waiting = None; k(o); case None => buffer.enqueue(o) } }(owner)
    }
    def next: Stream[O, Async] = new Stream[O, Async](() =>
      cancellable[O, Async] { cb =>
        subscribe()
        if (buffer.nonEmpty) { cb(Right(buffer.dequeue())); () => () }
        else { waiting = Some(o => cb(Right(o))); () => waiting = None }
      }.map(o => emitted(o, next)))
    next
  }

  /** an okay stream as an EventStream: pulled while observed, cancelled when no longer; pulled again from its start on
   * a restart (a stream is a value: pulled twice, it runs twice) */
  def toEventStream[O](s: Stream[O, Async]): EventStream[O] = {
    var running: Option[Running[Unit]] = None
    EventStream.fromCustomSource[O](
      start = (fire, fireError, _, _) => {
        val r = start(drain(s, fire))
        r.future.failed.foreach(fireError)
        running = Some(r)
      },
      stop = _ => { running.foreach(_.cancel()); running = None })
  }
  /** one program as an EventStream: its value one event, its failure one error, started when observed */
  def fromProgram[A](p: A ! Async): EventStream[A] = {
    var running: Option[Running[A]] = None
    EventStream.fromCustomSource[A](
      start = (fire, fireError, _, _) => {
        val r = start(p)
        r.future.foreach(fire); r.future.failed.foreach(fireError)
        running = Some(r)
      },
      stop = _ => { running.foreach(_.cancel()); running = None })
  }
  /** a stream drained into an Observer */
  def into[O](s: Stream[O, Async], observer: Observer[O]): Unit ! Async = drain(s, observer.onNext)
  private def drain[O](s: Stream[O, Async], fire: O => Unit): Unit ! Async =
    s.pull().flatMap(_.fold(pure[Unit, Async](()))((o, rest) => { fire(o); drain(rest, fire) }))

  /** a `Var[S]` as the handler of `State[S]`, in place: `get` is `now()`, `put` is `set` — the page sees every change
   */
  def varState[St](v: Var[St]): VarState[St] = new VarState[St](v)
  final class VarState[St](v: Var[St]) {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[({ type L[x] = State[St, x] })#L, R, Rest]): A ! Rest =
      handle[({ type L[x] = State[St, x] })#L, R, Rest, A, A](p,
        new TailHandler[({ type L[x] = State[St, x] })#L, Rest, A, A] {
          def ret(a: A) = pure(a)
          def answer[X](op: State[St, X]): X = op match {
            case okay.effects.Std.State.Get(answer) => answer(v.now())
            case okay.effects.Std.State.Put(s, answer) => v.set(s); answer(())
          }
        })
  }

  private def emitted[O](o: O, rest: => Stream[O, Async]): streams.Pull[O, Async] = new streams.Emit[O, Async](o, rest)
}

object AirStreams {
  def apply()(implicit fx: Runner, ec: ExecutionContext): AirStreams[fx.type, Streams[fx.type]] = {
    implicit val exact: fx.type = fx
    new AirStreams[fx.type, Streams[fx.type]](new Streams[fx.type])
  }
}
