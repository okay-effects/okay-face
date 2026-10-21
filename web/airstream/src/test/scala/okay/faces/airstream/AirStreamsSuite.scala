package okay.faces.airstream

import com.raquo.airstream.core.{EventStream, Observer}
import com.raquo.airstream.eventbus.EventBus
import com.raquo.airstream.ownership.ManualOwner
import com.raquo.airstream.state.Var
import okay.effects.{Sum, Void}
import okay.effects.async.Async
import okay.effects.machine.Machine
import okay.effects.stream.Streams
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/** THE BRIDGE, both ways: events pulled, a stream observed, a Var as a State */
class AirStreamsSuite extends munit.FunSuite {
  val streams = new Streams[Machine.type]()(Machine)
  val bridge = new AirStreams[Machine.type, streams.type](streams)
  import Machine._
  import Machine.std._
  import bridge.asynchrony.toFuture

  test("fromEventStream: the events as pulled, those between pulls kept in order, the subscription the owner's") {
    implicit val owner: ManualOwner = new ManualOwner
    val bus = new EventBus[Int]
    val s = bridge.fromEventStream(bus.events)
    val three = toFuture(s.take(3).toList)
    bus.writer.onNext(1); bus.writer.onNext(2); bus.writer.onNext(3); bus.writer.onNext(4)
    three.map { got => assertEquals(got, List(1, 2, 3)); owner.killSubscriptions() }
  }
  test("fromSignal: the current value first") {
    implicit val owner: ManualOwner = new ManualOwner
    val v = Var(10)
    val f = toFuture(bridge.fromSignal(v.signal).take(2).toList)
    v.set(11)
    f.map(got => assertEquals(got, List(10, 11)))
  }
  test("toEventStream: run while observed, cancelled when not; a value's stream observed twice runs twice") {
    var pulls = 0
    val s = streams.repeatEval[Int, Async](bridge.asynchrony.sleep[Async](1).map(_ => { pulls += 1; pulls }))
    val es = bridge.toEventStream(s)
    val owner = new ManualOwner
    val seen = scala.collection.mutable.ListBuffer.empty[Int]
    es.foreach(seen += _)(owner)
    Future(()).flatMap(_ => bridge.asynchrony.toFuture(bridge.asynchrony.sleep[Async](20))).map { _ =>
      owner.killSubscriptions()
      val n = seen.size
      assert(n >= 2, s"saw $n")
      assertEquals(seen.toList, (1 to n).toList)
    }.flatMap(_ => bridge.asynchrony.toFuture(bridge.asynchrony.sleep[Async](15))).map(_ =>
      assertEquals(seen.size, pulls)) // stopped: no more
  }
  test("fromProgram: one event, when the program is done; into: a stream drained into an observer") {
    val owner = new ManualOwner
    var got = Option.empty[Int]
    bridge.fromProgram(bridge.asynchrony.sleep[Async](1).map(_ => 42)).foreach(x => got = Some(x))(owner)
    val drained = scala.collection.mutable.ListBuffer.empty[Int]
    val into = toFuture(bridge.into(streams.emits[Int, Async](List(1, 2, 3)), Observer[Int](drained += _)))
    for { _ <- bridge.asynchrony.toFuture(bridge.asynchrony.sleep[Async](10)); _ <- into } yield {
      assertEquals(got, Some(42)); assertEquals(drained.toList, List(1, 2, 3))
    }
  }
  test("varState: a Var answers State in place; the page sees every put") {
    val v = Var(0)
    type S[X] = Sum[Void, ({ type L[x] = State[Int, x] })#L, X]
    val seen = scala.collection.mutable.ListBuffer.empty[Int]
    val owner = new ManualOwner
    v.signal.foreach(seen += _)(owner)
    val program: Int ! S = modify[Int, S](_ + 1).flatMap(_ => modify[Int, S](_ * 10)).flatMap(_ => get[Int, S])
    assertEquals(run(bridge.varState(v)(program)), 10)
    assertEquals(v.now(), 10)
    assertEquals(seen.toList, List(0, 1, 10))
  }
}
