package okay.faces.examples

import com.raquo.laminar.api.L._
import okay.effects.Void
import okay.effects.machine.Machine
import okay.faces.Sessions
import okay.faces.laminar.{LaminarFace, RemoteFace}
import okay.faces.rest.RestFace
import okay.http.{Request, Response}
import org.scalajs.dom
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/** THE LIST IN THE PAGE, under jsdom: the Laminar face handling it whole in the browser, and the remote face playing
 * it over the REST face's sessions — the form typed into, submitted, an item clicked, the list shown */
class TodoPageSuite extends munit.FunSuite {
  import Machine._
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
  def typeInto(root: dom.Element, id: String, value: String): Unit =
    q(root, s"#$id").asInstanceOf[dom.html.Input].value = value
  def submit(root: dom.Element, action: String): Unit =
    q(root, s"form[data-action=$action]").dispatchEvent(new dom.Event("submit",
        new dom.EventInit { bubbles = true; cancelable = true }))
  def settled: Future[Unit] = (1 to 8).foldLeft(Future.unit)((f, _) => f.flatMap(_ => Future(())))

  test("Laminar: the list in the page — two added, one clicked done, the marks and the count") {
    val face = new LaminarFace[Machine.type]()(Machine)
    val root = mounted(face.handle(Todo.keeping, LaminarFace.Settings(look = Some(Todo.look))))
    assertEquals(q(root, "h1").textContent, "To do")
    assert(root.innerHTML.contains("Nothing to do."), root.innerHTML)
    typeInto(root, "f-title", "milk"); submit(root, "add")
    typeInto(root, "f-title", "bread"); typeInto(root, "f-due", "2026-10-25"); submit(root, "add")
    assert(root.innerHTML.contains("added: bread") && root.innerHTML.contains("due 2026-10-25"), root.innerHTML)
    q(root, "a.okay-ref[href='#1']").click()
    assert(root.innerHTML.contains("1 of 2 done"), root.innerHTML)
    assert(root.querySelector(".okay-mark-done a.okay-ref") != null, root.innerHTML)
    assert(root.querySelector("button.okay-back") == null, "no back")
  }

  test("the remote face over the REST face: the same list, played through sessions") {
    // the sessions over the implementation alone (no Http in the row), each request answered on the machine
    val faces = new okay.faces.Faces[Machine.type]()(Machine)
    val service: Request => Response ! Void = new RestFace[Machine.type]()(Machine).sessions[Unit, Void](
      () => faces.session[Unit, Void](faces.steps[Unit, okay.faces.Face, Void](faces.of(Todo.keeping))),
      RestFace.Settings(ids = Sessions.counting("s"))).service
    var calls: List[Request] = Nil
    val transport: Request => Future[Response] =
      req => { calls = req :: calls; Future.successful(Machine.run(service(req))) }
    val root = mounted(RemoteFace(transport).element("/sessions"))
    settled.flatMap { _ =>
      assertEquals(q(root, "h1").textContent, "To do")
      typeInto(root, "f-title", "milk"); submit(root, "add")
      settled
    }.flatMap { _ =>
      assertEquals(calls.head.path, "/sessions/s1/act/add")
      assert(root.innerHTML.contains("added: milk"), root.innerHTML)
      q(root, "a.okay-ref[href='#1']").click()
      settled
    }.map { _ =>
      assertEquals(calls.head.path, "/sessions/s1/act/ref")
      assert(root.innerHTML.contains("1 of 1 done") && root.querySelector(".okay-mark-done") != null, root.innerHTML)
    }
  }
}
