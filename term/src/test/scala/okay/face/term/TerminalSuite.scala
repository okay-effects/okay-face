package okay.face.term

import okay.effects.{Runner, Sum, Void}
import okay.effects.machine.Machine
import okay.effects.free.Free
import okay.effects.shape.Shape
import okay.effects.shape.Shape.{field, int, record, text}
import okay.effects.world.Console
import okay.face.{Face, Faces, View}

/** THE TERMINAL FACE under a scripted console: what it prints, what it reads, on an implementation */
abstract class TerminalChecks(val fx0: Runner, name: String) extends munit.FunSuite {
  val faces: Faces[fx0.type] = new Faces[fx0.type]()(fx0)
  val term: Terminal[fx0.type] = new Terminal[fx0.type]()(faces)
  import fx0._
  import faces._
  import term.io.console

  type C[X] = Sum[Void, Console, X]
  type CF[X] = Sum[C, Face, X]
  final case class Who(name: String, age: Int)
  val who: Shape[Who] = record(field("name", text).labelled("Name"), field("age", int.check("of age")(_ >= 18)).documented("in years"))(Who(_, _))(w => (w.name, w.age))

  def greeting: String ! CF =
    fill[Who, CF](View.heading("Who are you?"), who).flatMap { w =>
      confirm[CF](View.text(s"Greet ${w.name}, ${w.age}?")).flatMap { yes =>
        if (yes) show[CF](View.text(s"Hello, ${w.name}!")).map(_ => s"greeted ${w.name}") else pure[String, CF]("left")
      }
    }
  def talk(inputs: String*): (List[String], Option[String]) = fx0.run(term.io.console(inputs.toList)(term.terminal[String, C](steps[String, CF, C](greeting))))

  test(s"$name: the dialogue — a view, a menu, the fields asked with their labels, a confirmation, the word at the end") {
    val (printed, result) = talk("1", "Ann", "41", "yes")
    assertEquals(result, Some("greeted Ann"))
    assertEquals(printed, List(
      "WHO ARE YOU?", "============", "",
      "1) Submit   (? help, q quit)", "> ",
      "Name: ", "age (a whole number, in years): ",
      "Greet Ann, 41?", "",
      "1) Yes  2) No   (< back, ? help, q quit)", "> ",
      "Hello, Ann!", ""))
  }
  test(s"$name: a problem shown and the field asked again; the action by name; quitting") {
    val (printed, result) = talk("submit", "Bo", "3", "20", "no")
    assertEquals(result, Some("left"))
    assert(printed.contains("problem: age: of age"), printed.mkString("\n"))
    assertEquals(printed.count(_ == "age (a whole number, in years): "), 2)
    assertEquals(talk("q")._2, None)
    assertEquals(talk()._2, None)
  }
  test(s"$name: back, help, and a choice that is not on the menu") {
    val (printed, result) = talk("3", "?", "1", "Ann", "41", "<", "1", "Cy", "30", "no")
    assertEquals(result, Some("left"))
    assert(printed.contains("problem: one of submit"))
    assert(printed.contains("submit: Submit"))
    assertEquals(printed.count(_ == "Greet Ann, 41?"), 1)
    assertEquals(printed.count(_ == "Greet Cy, 30?"), 1)
  }
  test(s"$name: a secret field is asked without echo, and says so") {
    final case class Login(user: String, pin: String)
    val login: Shape[Login] = record(field("user", text), field("pin", text).asSecret)(Login(_, _))(l => (l.user, l.pin))
    type LF[X] = Sum[C, Face, X]
    val p: Login ! LF = fill[Login, LF](View.text("Log in"), login)
    val (printed, result) = fx0.run(term.io.console(List("1", "ann", "1234"))(term.terminal[Login, C](steps[Login, LF, C](p))))
    assertEquals(result, Some(Login("ann", "1234")))
    assert(printed.contains("pin (hidden): "), printed.mkString("\n"))
  }
  test(s"$name: rendering — headings, fields, tables, items, groups, wrapping") {
    assertEquals(Render.view(View.heading("Hi", 2)), List("Hi", "--"))
    assertEquals(Render.view(View.fields("name" -> View.text("Ann"), "born" -> View.text("1985"))), List("name: Ann", "born: 1985"))
    assertEquals(Render.view(View.table("a", "bb")(Vector(View.text("1"), View.text("2")), Vector(View.text("333"), View.text("4")))),
      List("a    bb", "---  --", "1    2", "333  4"))
    assertEquals(Render.view(View.items(View.text("x"), View.text("y"))), List("- x", "- y"))
    assertEquals(Render.view(View.group("G")(View.text("x"))), List("## G", "  x"))
    assertEquals(Render.view(View.problem("no")), List("problem: no"))
    assertEquals(Render.wrap("a " * 50, 20).forall(_.length <= 20), true)
    assertEquals(Render.view(View.Masked("•••• 4821")), List("•••• 4821"))
  }
}

class TerminalOnMachine extends TerminalChecks(Machine, "machine")
class TerminalOnFree extends TerminalChecks(Free, "free")

/** the help wrapped: lines of at most the width, the rest indented under the text */
class WrapSuite extends munit.FunSuite {
  test("words onto lines of at most the width; continuation indented; a long word alone") {
    val lines = Terminal.wrap("ref: " + ("word " * 30).trim, 40, 5)
    assert(lines.forall(_.length <= 40), lines.mkString("\n"))
    assert(lines.head.startsWith("ref: word") && lines.tail.forall(_.startsWith("     word")), lines.mkString("\n"))
    assertEquals(Terminal.wrap("a " + "x" * 50 + " b", 20), List("a", "x" * 50, "b"))
    assertEquals(Terminal.wrap("short", 80), List("short"))
  }
}
