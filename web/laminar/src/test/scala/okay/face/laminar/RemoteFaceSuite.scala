package okay.face.laminar

import com.raquo.laminar.api.L._
import okay.effects.{Sum, Void}
import okay.effects.machine.Machine
import okay.effects.shape.Shape
import okay.effects.shape.Shape.{field, int, record, text}
import okay.face.{Face, Faces, View}
import okay.face.rest.RestFace
import okay.http.{Request, Response}
import org.scalajs.dom
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/** THE REMOTE FACE under jsdom, its server the REST face in the same process: the dialogue runs there, the page here */
class RemoteFaceSuite extends munit.FunSuite {
  val faces: Faces[Machine.type] = new Faces[Machine.type]()(Machine)
  import Machine._
  import faces._
  type F[X] = Sum[Void, Face, X]

  final case class Who(name: String, age: Int)
  val who: Shape[Who] = record(field("name", text).labelled("Name"), field("age", int.check("of age")(_ >= 18)))(Who(_, _))(w => (w.name, w.age))
  def greeting: String ! F =
    fill[Who, F](View.heading("Who are you?"), who).flatMap { w =>
      confirm[F](View.text(s"Greet ${w.name}, ${w.age}?")).flatMap { yes =>
        if (yes) show[F](View.text(s"Hello, ${w.name}!")).map(_ => s"greeted ${w.name}") else pure[String, F]("left")
      }
    }
  /** the server: the REST face's sessions, each request answered on the machine; the transport, a future of it */
  val service: Request => Response ! Void = RestFace(faces).sessions[String, Void](() => faces.dialogue[String, Void](steps[String, F, Void](greeting))).service
  var calls: List[Request] = Nil
  val transport: Request => Future[Response] = req => { calls = req :: calls; Future.successful(Machine.run(service(req))) }
  val face = RemoteFace(transport)

  def mounted(el: HtmlElement): dom.html.Element = {
    dom.document.body.innerHTML = ""
    val container = dom.document.createElement("div").asInstanceOf[dom.html.Element]
    dom.document.body.appendChild(container)
    render(container, el)
    container
  }
  def q(root: dom.Element, selector: String): dom.html.Element = {
    val e = root.querySelector(selector)
    assert(e != null, s"no $selector in: ${root.innerHTML}")
    e.asInstanceOf[dom.html.Element]
  }
  def typeInto(root: dom.Element, id: String, value: String): Unit = q(root, s"#$id").asInstanceOf[dom.html.Input].value = value
  def submit(root: dom.Element, action: String): Unit =
    q(root, s"form[data-action=$action]").dispatchEvent(new dom.Event("submit", new dom.EventInit { bubbles = true; cancelable = true }))
  /** the futures of a call settle a few ticks later */
  def settled: Future[Unit] = (1 to 8).foldLeft(Future.unit)((f, _) => f.flatMap(_ => Future(())))

  test("the session opened, the form from the schema that came, the server's problems, the dialogue to its end") {
    val root = mounted(face.element("/sessions"))
    settled.flatMap { _ =>
      assertEquals(calls.last.method, "POST"); assertEquals(calls.last.path, "/sessions")
      assertEquals(q(root, "h1").textContent, "Who are you?")
      assertEquals(q(root, "label[for=f-name]").textContent, "Name")
      typeInto(root, "f-name", "Ann"); typeInto(root, "f-age", "3")
      submit(root, "submit")
      settled
    }.flatMap { _ =>
      assert(root.innerHTML.contains("age: of age"), root.innerHTML)         // the server's 400: the shape's check is there
      assertEquals(q(root, "h1").textContent, "Who are you?")               // the same step
      assertEquals(calls.head.path, "/sessions/s1/act/submit")
      typeInto(root, "f-name", "Ann"); typeInto(root, "f-age", "41")
      submit(root, "submit")
      settled
    }.flatMap { _ =>
      assertEquals(q(root, "p").textContent, "Greet Ann, 41?")
      assert(root.querySelector("button.okay-back") != null)
      q(root, "button[data-action=yes]").click()
      settled
    }.map { _ =>
      assert(root.innerHTML.contains("Hello, Ann!") && root.innerHTML.contains("okay-done"), root.innerHTML)
      assert(root.querySelector("form") == null)
    }
  }
  test("a choice checked here before it travels: a missing field is a problem without a call") {
    val root = mounted(face.element("/sessions"))
    settled.flatMap { _ =>
      val before = calls.size
      submit(root, "submit")
      settled.map { _ =>
        assert(root.innerHTML.contains("name"), root.innerHTML)
        assertEquals(calls.size, before)
      }
    }
  }
  test("back is the server's") {
    val root = mounted(face.element("/sessions"))
    settled.flatMap { _ =>
      typeInto(root, "f-name", "Bo"); typeInto(root, "f-age", "20"); submit(root, "submit"); settled
    }.flatMap { _ =>
      assertEquals(q(root, "p").textContent, "Greet Bo, 20?")
      q(root, "button.okay-back").click(); settled
    }.map { _ =>
      assertEquals(q(root, "h1").textContent, "Who are you?")
      assert(calls.head.path.endsWith("/back"))
    }
  }
}
