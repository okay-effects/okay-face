package okay.faces.swing

import java.awt.{Component, Container}
import javax.swing._
import okay.effects.{Sum, Void}
import okay.effects.machine.Machine
import okay.effects.shape.Shape
import okay.effects.shape.Shape.{field, int, record, text}
import okay.faces.{Face, Faces, View}

/** THE SWING FACE without a display: components built and driven in headless mode */
class SwingFaceSuite extends munit.FunSuite {
  System.setProperty("java.awt.headless", "true")
  val faces: Faces[Machine.type] = new Faces[Machine.type]()(Machine)
  val face = new SwingFace[Machine.type]()(faces)
  import Machine._
  import faces._
  type F[X] = Sum[Void, Face, X]

  final case class Who(name: String, age: Int)
  val who: Shape[Who] =
    record(field("name", text).labelled("Name"), field("age", int.check("of age")(_ >= 18)))(Who(_, _))(w =>
      (w.name, w.age))
  def greeting: String ! F =
    fill[Who, F](View.heading("Who are you?"), who).flatMap { w =>
      confirm[F](View.text(s"Greet ${w.name}, ${w.age}?")).flatMap { yes =>
        if (yes) show[F](View.text(s"Hello, ${w.name}!")).map(_ => s"greeted ${w.name}") else pure[String, F]("left")
      }
    }

  def all(c: Component): List[Component] = c match {
    case p: Container => c :: p.getComponents.toList.flatMap(all)
    case _ => List(c)
  }
  def byName[C <: Component](root: Component, name: String): C = all(root).find(_.getName == name).getOrElse(
    fail(s"no $name among ${all(root).flatMap(x => Option(x.getName))}")).asInstanceOf[C]
  def texts(root: Component): List[String] =
    all(root).collect { case l: JLabel => l.getText; case a: JTextArea => a.getText }

  test("a form from the shape, filled and submitted; a problem shown; a button; the end") {
    var result = Option.empty[String]
    val root = face.panel(steps[String, F, Void](greeting), (a: String) => result = Some(a))
    assert(texts(root).contains("Who are you?"))
    byName[JTextField](root, "f-name").setText("Ann"); byName[JTextField](root, "f-age").setText("3")
    byName[JButton](root, "submit:submit").doClick()
    assert(texts(root).contains("age: of age"), texts(root))
    byName[JTextField](root, "f-name").setText("Ann"); byName[JTextField](root, "f-age").setText("41")
    byName[JButton](root, "submit:submit").doClick()
    assert(texts(root).contains("Greet Ann, 41?"), texts(root))
    byName[JButton](root, "action:yes").doClick()
    assert(texts(root).contains("Hello, Ann!"), texts(root))
    assertEquals(result, Some("greeted Ann"))
  }
  test("back re-resumes the previous step") {
    val root = face.panel(steps[String, F, Void](greeting))
    byName[JTextField](root, "f-name").setText("Bo"); byName[JTextField](root, "f-age").setText("20")
    byName[JButton](root, "submit:submit").doClick()
    assert(texts(root).contains("Greet Bo, 20?"))
    byName[JButton](root, "okay-back").doClick()
    assert(texts(root).contains("Who are you?"))
    assert(all(root).forall(_.getName != "okay-back"))
  }
  test("views as components: table, fields, items, masked") {
    val v = View.stack(View.table("a", "b")(Vector(View.text("1"), View.Masked("••"))),
      View.fields("k" -> View.text("v")), View.items(View.text("x")))
    val root = face.panel(steps[Unit, F, Void](show[F](v)))
    assertEquals(byName[JTable](root, "okay-table").getColumnCount, 2)
    assertEquals(byName[JTable](root, "okay-table").getValueAt(0, 1), "••")
    assert(texts(root).contains("k:"))
    assertEquals(byName[JList[String]](root, "okay-items").getModel.getElementAt(0), "x")
  }
  test("an event while the step waits: fired, the panel shows the rest, on the event dispatch thread") {
    val bus = okay.faces.Meanwhile.manual[Int]
    val program: String ! F =
      offer[Int, F](View.text("waiting"), List(okay.faces.Action.plain[Int]("skip", "Skip")(0)), bus.meanwhile).flatMap(
        n => show[F](View.text(s"got $n")).map(_ => s"$n"))
    var result = Option.empty[String]
    val root = face.panel(steps[String, F, Void](program), (a: String) => result = Some(a))
    assert(texts(root).contains("waiting"))
    bus.fire(9)
    SwingUtilities.invokeAndWait(new Runnable { def run(): Unit = () })
    assert(texts(root).contains("got 9"), texts(root).toString); assertEquals(result, Some("9"));
    assertEquals(bus.listening, 0)
  }
  test("a list of records in a form: an item added, filled, one removed, submitted as the records") {
    final case class Line(what: String, qty: Int)
    final case class Order(who: String, lines: List[Line])
    val line = record(field("what", text), field("qty", int))(Line(_, _))(l => (l.what, l.qty))
    val order: Shape[Order] =
      record(field("who", text), field("lines", line.many()))(Order(_, _))(o => (o.who, o.lines))
    var got = Option.empty[Order]
    val root =
      face.panel(steps[Order, F, Void](fill[Order, F](View.heading("Order"), order)), (o: Order) => got = Some(o))
    byName[JTextField](root, "f-who").setText("ann")
    byName[JButton](root, "add:f-lines").doClick(); byName[JButton](root, "add:f-lines").doClick();
    byName[JButton](root, "add:f-lines").doClick()
    byName[JTextField](root, "f-lines-0-what").setText("tea"); byName[JTextField](root, "f-lines-0-qty").setText("2")
    byName[JTextField](root, "f-lines-1-what").setText("gone"); byName[JTextField](root, "f-lines-1-qty").setText("9")
    byName[JTextField](root, "f-lines-2-what").setText("milk"); byName[JTextField](root, "f-lines-2-qty").setText("1")
    byName[JButton](root, "remove:f-lines-1").doClick()
    byName[JButton](root, "submit:submit").doClick()
    assertEquals(got, Some(Order("ann", List(Line("tea", 2), Line("milk", 1)))))
  }

  test("the help: the views told before the first step as one component, every text there, named as told") {
    val h = SwingFace.help(Vector(View.text("You are X"), View.text("Three in a row wins")))
    def all(c: java.awt.Component): List[java.awt.Component] =
      c match { case p: java.awt.Container => c :: p.getComponents.toList.flatMap(all); case _ => List(c) }
    val texts = all(h).collect { case a: javax.swing.JTextArea => a.getText; case l: javax.swing.JLabel => l.getText }
    assert(texts.exists(_.contains("You are X")) && texts.exists(_.contains("Three in a row")), texts.toString)
    assertEquals(h.getName, "okay-told")
  }
}
