package okay.faces.swing

import java.awt.{Color => AwtColor, Component, Container, Font}
import javax.swing._
import okay.effects.{Sum, Void}
import okay.effects.machine.Machine
import okay.faces.{Face, Faces, Look, Role, View}

/** THE LOOK on Swing: the components dressed by their roles' rules, headless */
class LooksSuite extends munit.FunSuite {
  System.setProperty("java.awt.headless", "true")
  val faces: Faces[Machine.type] = new Faces[Machine.type]()(Machine)
  val face = new SwingFace[Machine.type]()(faces)
  import Machine._
  import faces._
  type F[X] = Sum[Void, Face, X]
  def all(c: Component): List[Component] =
    c match { case p: Container => c :: p.getComponents.toList.flatMap(all); case _ => List(c) }
  def byName[C <: Component](root: Component, name: String): C = all(root).find(_.getName == name).getOrElse(
    fail(s"no $name among ${all(root).flatMap(x => Option(x.getName))}")).asInstanceOf[C]
  def awt(t: okay.faces.Tone): AwtColor = { val c = Look.default.theme.palette(t); new AwtColor(c.r, c.g, c.b) }

  val program: Unit ! F =
    confirm[F](View.stack(View.heading("Title"), View.problem("Wrong"), View.Masked("•• 12"))).map(_ => ())

  test(
    "with a look: the heading bold and larger, the problem in its tone, the face on its background, " +
      "the button an accent") {
    val root = face.panel(steps[Unit, F, Void](program), (_: Unit) => (), Some(Look.default))
    val h = byName[JLabel](root, Role.Heading(1).cls)
    assert(h.getFont.isBold); assertEquals(h.getFont.getSize, 26)
    assertEquals(byName[JLabel](root, Role.Notice(View.Notice.Problem).cls).getForeground, awt(okay.faces.Tone.Problem))
    assertEquals(byName[JLabel](root, Role.Masked.cls).getFont.getFamily,
      new Font(Font.MONOSPACED, Font.PLAIN, 1).getFamily)
    assertEquals(root.getBackground, awt(okay.faces.Tone.Bg)); assert(root.isOpaque)
    assertEquals(byName[JButton](root, "action:yes").getBackground, awt(okay.faces.Tone.Accent))
  }
  test("without a look: the platform's fonts stand") {
    val root = face.panel(steps[Unit, F, Void](program))
    assertNotEquals(byName[JLabel](root, Role.Heading(1).cls).getFont.getSize, 26)
    assertEquals(Looks(Look.default).roleOf(byName[JButton](root, "action:yes")), Some(Role.Action))
  }
}
