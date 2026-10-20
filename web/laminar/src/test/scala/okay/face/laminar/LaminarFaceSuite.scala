package okay.face.laminar

import com.raquo.laminar.api.L._
import okay.effects.{Sum, Void}
import okay.effects.machine.Machine
import okay.effects.shape.Shape
import okay.effects.shape.Shape.{field, int, record, sub, sum, text, unit}
import okay.effects.async.Async
import okay.face.{Action, Face, Faces, View}
import org.scalajs.dom
import scala.concurrent.{Future, Promise}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

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
    // one face in the document at a time: jsdom answers an id selector from the whole document, then checks the subtree
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
  test("a sum in a form: the case chosen by a select, its fields the ones collected") {
    sealed trait Pet
    final case class Dog(name: String) extends Pet
    final case class Fish(litres: Int) extends Pet
    val pet: Shape[Pet] = sum(
      sub[Pet, Dog]("dog", record(field("name", text))(Dog(_))(_.name)) { case d: Dog => Some(d); case _ => None },
      sub[Pet, Fish]("fish", record(field("litres", int))(Fish(_))(_.litres)) { case f: Fish => Some(f); case _ => None })
    var got = Option.empty[Pet]
    val root = mounted(face.element(steps[Pet, F, Void](fill[Pet, F](View.text("A pet"), pet)), (p: Pet) => got = Some(p)))
    val sel = q(root, "#f").asInstanceOf[dom.html.Select]
    sel.value = "fish"; sel.dispatchEvent(new dom.Event("change", new dom.EventInit { bubbles = true }))
    typeInto(root, "f-fish-litres", "20")
    submit(root, "submit")
    assertEquals(got, Some(Fish(20)))
  }
  test("Async in the rest: a step that waits, the next view when it is done") {
    type AF[X] = Sum[Async, Face, X]
    val asynchrony = new okay.effects.async.Asynchrony[Machine.type]()(Machine, queue)
    def waiting: String ! AF = offer[Unit, AF](View.text("Press"), List(Action.plain[Unit]("go", "Go")(()))).flatMap(_ =>
      asynchrony.sleep[AF](5)).flatMap(_ => show[AF](View.text("Waited")).map(_ => "done"))
    val ended = Promise[String]()
    val root = mounted(face.elementAsync(steps[String, AF, Async](waiting), (a: String) => ended.success(a)))
    // the first step arrives when its future completes: a tick later
    Future(()).flatMap { _ =>
      assertEquals(q(root, "p").textContent, "Press")
      press(root, "go")
      // while the rest runs, the step stays shown with its actions disabled
      assert(q(root, "button[data-action=go]").hasAttribute("disabled"), root.innerHTML)
      ended.future.map { a =>
        assertEquals(a, "done")
        assert(root.innerHTML.contains("Waited"), root.innerHTML)
      }
    }
  }
  test("the browser's back: a popstate re-resumes the previous step") {
    val root = mounted(face.element(steps[String, F, Void](greeting), browserHistory = true))
    typeInto(root, "f-name", "Di"); typeInto(root, "f-age", "33")
    submit(root, "submit")
    assertEquals(q(root, "p").textContent, "Greet Di, 33?")
    dom.window.dispatchEvent(new dom.Event("popstate"))
    assertEquals(q(root, "h1").textContent, "Who are you?")
  }
}

class LifecycleSuite extends munit.FunSuite {
  import okay.effects.machine.Machine
  import okay.effects.async.Async
  import scala.concurrent.Promise
  import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
  val life = new Lifecycle[Machine.type]()(Machine, queue)
  import life.asynchrony.sleep
  def mount(el: HtmlElement): RootNode = { val c = dom.document.createElement("div").asInstanceOf[dom.html.Element]; dom.document.body.appendChild(c); render(c, el) }

  test("a program runs when the element mounts, and its result arrives") {
    val got = Promise[Int]()
    mount(div(life.runOnMount(sleep[Async](1).map(_ => 7))(r => got.complete(r.toTry))))
    got.future.map(assertEquals(_, 7))
  }
  test("unmounted before it is done: cancelled, nothing arrives") {
    var arrived = false
    val root = mount(div(life.runOnMount(sleep[Async](30).map(_ => 1))(_ => arrived = true)))
    root.unmount()   // Laminar's own unmount: the callbacks fire (a removal by innerHTML would not tell Laminar)
    life.asynchrony.toFuture(sleep[Async](50)).map(_ => assert(!arrived))
  }
}
