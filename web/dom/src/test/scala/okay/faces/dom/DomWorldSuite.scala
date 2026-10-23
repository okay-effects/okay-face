package okay.faces.dom

import okay.effects.{Sum, Void}
import okay.effects.machine.Machine
import okay.effects.shape.Shape
import okay.effects.shape.Shape.{field, int, record, text}
import okay.faces.{Face, Faces, View}
import org.scalajs.dom

/** THE DOM FACE'S OWN: its world as effects — the keyboard where a person needs it, the problems announced, Escape as
 * back, busy told, and every operation on the page one a face may intercept */
class DomWorldSuite extends munit.FunSuite {
  val faces: Faces[Machine.type] = new Faces[Machine.type]()(Machine)
  import Machine._
  import faces._
  type F[X] = Sum[Void, Face, X]

  final case class Who(name: String, age: Int)
  val who: Shape[Who] = record(field("name", text).labelled("Name").documented("as you are called"),
    field("age", int.check("of age")(_ >= 18)))(Who(_, _))(w => (w.name, w.age))
  def greeting: String ! F =
    fill[Who, F](View.heading("Who are you?"), who, okay.faces.Step.Options.returnable).flatMap { w =>
      confirm[F](View.text(s"Greet ${w.name}, ${w.age}?")).map(yes => if (yes) "greeted" else "left")
    }

  def inPage(el: dom.html.Element): dom.html.Element = {
    dom.document.body.innerHTML = ""
    dom.document.body.appendChild(el)
    el
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
  def active: dom.Element = dom.document.activeElement

  test("mounted, the first field takes the keyboard; a problem's field marked, announced, and focused") {
    dom.document.body.innerHTML = ""
    new DomFace[Machine.type]()(Machine).mount(okay.faces.fill[Who, Face](View.heading("Who are you?"), who)(
      okay.faces.splitOne[Face]))
    val root = dom.document.body
    assertEquals(active.id, "f-name")
    assertEquals(q(root, "#f-name").getAttribute("aria-description"), "as you are called")
    typeInto(root, "f-name", "Ann"); typeInto(root, "f-age", "3"); submit(root, "submit")
    assertEquals(q(root, "ul.okay-problems").getAttribute("role"), "alert")
    assertEquals(q(root, "#f-age").getAttribute("aria-invalid"), "true")
    assertEquals(q(root, "#f-name").getAttribute("aria-invalid"), null)
    assertEquals(active.id, "f-age")
    assertEquals(q(root, "#f-name").asInstanceOf[dom.html.Input].value, "Ann") // what was typed stays
  }

  test("a step with no field: its first action takes the keyboard; Escape is back; busy is told while it is") {
    val face = new DomFace[Machine.type]()(Machine)
    val root = inPage(face.element(steps[String, F, Void](greeting)))
    assertEquals(root.getAttribute("aria-busy"), null) // the face itself: nothing running
    typeInto(root, "f-name", "Bo"); typeInto(root, "f-age", "20"); submit(root, "submit")
    assertEquals(q(root, "p").textContent, "Greet Bo, 20?")
    assertEquals(active.getAttribute("data-action"), "yes")
    active.dispatchEvent(new dom.KeyboardEvent("keydown",
      new dom.KeyboardEventInit { key = "Escape"; bubbles = true; cancelable = true }))
    assertEquals(q(root, "h1").textContent, "Who are you?")
    assertEquals(active.id, "f-name")
  }

  test("focus off: the face leaves the keyboard where it is") {
    val face = new DomFace[Machine.type]()(Machine)
    val root = inPage(face.element(steps[String, F, Void](greeting), settings = DomFace.Settings(focus = false)))
    typeInto(root, "f-name", "Bo"); typeInto(root, "f-age", "20"); submit(root, "submit")
    assert(active == dom.document.body, active.outerHTML)
  }

  test("every operation on the page is the face's effect: intercepted, each one seen before it is done") {
    var made = List.empty[String]
    var focused = 0
    val face = new DomFace[Machine.type]()(Machine) {
      import browser.W
      override def world[X](p: X ! W): X = browser.world(
        Machine.intercept[Focus, W, X, X](Machine.intercept[Page, W, X, X](p, new Handler[Page, W, X, X] {
          def ret(x: X) = pure(x)
          def op[Y](op: Page[Y], k: Y => X ! W) = {
            op match { case Page.Element(tag, _, _, _) => made = tag :: made; case _ => () }
            perform[Page].apply[W, Y](op).flatMap(k)
          }
        }), new Handler[Focus, W, X, X] {
          def ret(x: X) = pure(x)
          def op[Y](op: Focus[Y], k: Y => X ! W) = { focused += 1; perform[Focus].apply[W, Y](op).flatMap(k) }
        }))
    }
    val root = inPage(face.element(steps[String, F, Void](greeting)))
    assert(made.contains("form") && made.contains("input") && made.contains("h1"), made.toString)
    typeInto(root, "f-name", "Bo"); typeInto(root, "f-age", "20"); submit(root, "submit")
    assert(focused > 0)
    assertEquals(q(root, "p").textContent, "Greet Bo, 20?")
  }
}
