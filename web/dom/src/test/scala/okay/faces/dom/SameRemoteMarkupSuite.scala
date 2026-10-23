package okay.faces.dom

import okay.effects.{Sum, Void}
import okay.effects.machine.Machine
import okay.effects.shape.Shape
import okay.effects.shape.Shape.{field, int, record, text}
import okay.faces.{Face, Faces, View}
import okay.faces.laminar.RemoteFace
import okay.faces.rest.RestFace
import okay.http.{Request, Response}
import org.scalajs.dom
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/**
 * THE SAME MARKUP, REMOTE: one dialogue on a server (the REST face, in the process), its page drawn by the Laminar remote
 * face and by the DOM's, the same acts, and the page's markup compared after each act has settled — Laminar's comment
 * nodes and the DOM face's accessibility attributes left out, as in `SameMarkupSuite`.
 */
class SameRemoteMarkupSuite extends munit.FunSuite {
  val faces: Faces[Machine.type] = new Faces[Machine.type]()(Machine)
  import Machine._
  import faces._
  type F[X] = Sum[Void, Face, X]

  final case class Who(name: String, age: Int)
  val who: Shape[Who] = record(field("name", text).labelled("Name"), field("age", int.check("of age")(_ >= 18)))(
    Who(_, _))(w => (w.name, w.age))
  def greeting: String ! F =
    fill[Who, F](View.heading("Who are you?"), who, okay.faces.Step.Options.returnable).flatMap { w =>
      confirm[F](View.text(s"Greet ${w.name}, ${w.age}?")).flatMap { yes =>
        if (yes) show[F](View.text(s"Hello, ${w.name}!")).map(_ => s"greeted ${w.name}") else pure[String, F]("left")
      }
    }
  /** a server of its own for each page: the same session ids, so the same addresses */
  def transport(): Request => Future[Response] = {
    val service: Request => Response ! Void = new RestFace[Machine.type]()(Machine).sessions[String, Void](() =>
        faces.session[String, Void](steps[String, F, Void](greeting)),
      RestFace.Settings(ids = okay.faces.Sessions.counting("s"))).service
    req => Future.successful(Machine.run(service(req)))
  }
  def settled: Future[Unit] = (1 to 8).foldLeft(Future.unit)((f, _) => f.flatMap(_ => Future(())))

  def markup(root: dom.Node): String = {
    val copy = root.cloneNode(true)
    def strip(n: dom.Node): Unit = {
      var c = n.firstChild
      while (c != null) {
        val next = c.nextSibling
        if (c.nodeType == dom.Node.COMMENT_NODE) n.removeChild(c)
        else {
          if (c.nodeType == dom.Node.ELEMENT_NODE) {
            val e = c.asInstanceOf[dom.Element]
            (0 until e.attributes.length).map(i => e.attributes(i).name)
              .filter(a => a.startsWith("aria-") || a == "role").foreach(e.removeAttribute)
          }
          strip(c)
        }
        c = next
      }
    }
    strip(copy)
    copy.asInstanceOf[dom.html.Element].innerHTML
  }
  def q(root: dom.Element, selector: String): dom.html.Element = {
    val e = root.querySelector(selector)
    assert(e != null, s"no $selector in: ${root.innerHTML}")
    e.asInstanceOf[dom.html.Element]
  }
  def typeInto(id: String, value: String): dom.html.Element => Unit =
    root => q(root, s"#$id").asInstanceOf[dom.html.Input].value = value
  def submit(action: String): dom.html.Element => Unit = root =>
    q(root, s"form[data-action=$action]").dispatchEvent(new dom.Event("submit",
        new dom.EventInit { bubbles = true; cancelable = true }))
  def press(selector: String): dom.html.Element => Unit = root => q(root, selector).click()
  def both(acts: (dom.html.Element => Unit)*): dom.html.Element => Unit = root => acts.foreach(_(root))

  /** the acts: refused here (nothing typed), refused by the server, taken, back, taken again, yes */
  val acts: List[dom.html.Element => Unit] = List(
    submit("submit"),
    both(typeInto("f-name", "Ann"), typeInto("f-age", "3"), submit("submit")),
    both(typeInto("f-age", "41"), submit("submit")),
    press("button.okay-back"),
    both(typeInto("f-name", "Bo"), typeInto("f-age", "20"), submit("submit")),
    press("button[data-action=yes]"))

  /** the page made by `start`, its markup once settled, then after each act settled */
  def played(start: dom.html.Element => Unit): Future[List[String]] = {
    dom.document.body.innerHTML = ""
    val c = dom.document.createElement("div").asInstanceOf[dom.html.Element]
    dom.document.body.appendChild(c)
    start(c)
    acts.foldLeft(settled.map(_ => List(markup(c)))) { (seen, act) =>
      seen.flatMap(ms => { act(c); settled.map(_ => ms :+ markup(c)) })
    }
  }

  test("the same markup — a dialogue on the server: refused here, refused there, taken, back, to its end") {
    played { c =>
      com.raquo.laminar.api.L.render(c, RemoteFace(transport()).element("/sessions")); ()
    }.flatMap { l =>
      played(c => { c.appendChild(DomRemoteFace[Machine.type](transport())(Machine).element("/sessions")); () }).map { d =>
        assertEquals(d.size, l.size)
        // the dialogue went where the acts took it: refused, refused, taken, back, taken, to its end
        assert(l(1).contains("name") && l(2).contains("of age") && l(3).contains("Greet Ann, 41?") &&
          l(4).contains("Who are you?") && l.last.contains("Hello, Bo!"), l.mkString("\n"))
        l.zip(d).zipWithIndex.foreach { case ((a, b), i) => assertEquals(b, a, s"after act $i") }
      }
    }
  }
}
