package okay.face.laminar

import com.raquo.laminar.api.L._
import okay.effects.{Sum, Void}
import okay.effects.machine.Machine
import okay.effects.shape.Shape
import okay.effects.shape.Shape.{field, int, record, text}
import okay.face.{Action, Face, Faces, View}
import org.scalajs.dom

/** THE LAMINAR FACE under jsdom: the view drawn, a form filled and submitted, a problem shown, a button pressed, back */
class LaminarFaceSuite extends munit.FunSuite {
  val faces: Faces[Machine.type] = new Faces[Machine.type]()(Machine)
  val face = new LaminarFace[Machine.type](faces)
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

  def mounted(el: HtmlElement): dom.html.Element = {
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
  def submit(root: dom.Element, action: String): Unit = {
    val form = q(root, s"form[data-action=$action]").asInstanceOf[dom.html.Form]
    form.dispatchEvent(new dom.Event("submit", new dom.EventInit { bubbles = true; cancelable = true }))
  }
  def press(root: dom.Element, action: String): Unit = q(root, s"button[data-action=$action]").click()

  test("a form from the shape, filled and submitted; a problem shown; the dialogue to its end") {
    var result = Option.empty[String]
    val root = mounted(face.element(steps[String, F, Void](greeting), (a: String) => result = Some(a)))
    assertEquals(q(root, "h1").textContent, "Who are you?")
    assertEquals(q(root, "label[for=f-name]").textContent, "Name")
    assertEquals(q(root, "#f-age").getAttribute("type"), "number")
    typeInto(root, "f-name", "Ann"); typeInto(root, "f-age", "3")
    submit(root, "submit")
    assert(root.innerHTML.contains("age: of age"), root.innerHTML)        // the problem, from the shape
    assertEquals(q(root, "h1").textContent, "Who are you?")              // the same step
    typeInto(root, "f-name", "Ann"); typeInto(root, "f-age", "41")
    submit(root, "submit")
    assertEquals(q(root, "p").textContent, "Greet Ann, 41?")
    press(root, "yes")
    assert(root.innerHTML.contains("Hello, Ann!"), root.innerHTML)
    assertEquals(result, Some("greeted Ann"))
  }
  test("back re-resumes the previous step") {
    val root = mounted(face.element(steps[String, F, Void](greeting)))
    typeInto(root, "f-name", "Bo"); typeInto(root, "f-age", "20")
    submit(root, "submit")
    assertEquals(q(root, "p").textContent, "Greet Bo, 20?")
    q(root, "button.okay-back").click()
    assertEquals(q(root, "h1").textContent, "Who are you?")
    assert(root.querySelector("button.okay-back") == null)
  }
  test("views drawn: table, fields, items, masked") {
    val v = View.stack(View.table("a", "b")(Vector(View.text("1"), View.Masked("••"))), View.fields("k" -> View.text("v")), View.items(View.text("x")))
    val root = mounted(face.element(steps[Unit, F, Void](show[F](v))))
    assertEquals(root.querySelectorAll("th").length, 2)
    assertEquals(q(root, "span.okay-masked").textContent, "••")
    assertEquals(q(root, "dt").textContent, "k")
    assertEquals(q(root, "li").textContent, "x")
  }
}
