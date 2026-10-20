package okay.face.waypoint

import com.raquo.laminar.api.L._
import com.raquo.waypoint._
import okay.effects.{Sum, Void}
import okay.effects.async.Async
import okay.effects.machine.Machine
import okay.effects.stream.Streams
import okay.face.airstream.AirStreams
import okay.face.{Navigation, Navigations}
import org.scalajs.dom
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

class NavigationSuite extends munit.FunSuite {
  val streams = new Streams[Machine.type]()(Machine)
  val bridge = new AirStreams[Machine.type, streams.type](streams)
  val routes = new Routes[Machine.type, streams.type](bridge)
  val nav = new Navigations[Machine.type]()(Machine)
  import Machine._
  import nav.{push, replace, current, recording}
  import routes.router

  sealed trait Page
  case object Home extends Page
  final case class User(id: Int) extends Page
  type N[X] = Navigation[Page, X]
  type R[X] = Sum[Void, N, X]

  def visit: Page ! R = push[Page, R](User(1)).flatMap(_ => push[Page, R](User(2))).flatMap(_ => replace[Page, R](User(3))).flatMap(_ => current[Page, R])

  test("recording: the pages asked for, replace replacing the last, current the last") {
    assertEquals(run(recording[Page](Home)(visit)), (List(Home, User(1), User(3)), User(3): Page))
  }
  test("Waypoint's router: pushState changes the current page and the URL") {
    val origin = dom.window.location.origin   // jsdom's document is http://localhost/
    val table = List(
      Route.static(Home, root / endOfSegments),
      Route[User, Int](encode = _.id, decode = User(_), pattern = root / "user" / segment[Int] / endOfSegments))
    val r = new Router[Page](routes = table, serializePage = { case Home => "home"; case User(i) => s"user:$i" },
      deserializePage = s => if (s == "home") Home else User(s.drop(5).toInt), getPageTitle = _.toString,
      routeFallback = _ => Home, origin = origin, initialUrl = dom.window.location.href)
    assertEquals(run(router(r)(visit)), User(3): Page)
    assertEquals(r.currentPageSignal.now(), User(3): Page)
    assert(dom.window.location.href.endsWith("/user/3"), dom.window.location.href)
  }
}
