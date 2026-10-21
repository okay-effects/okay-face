package okay.faces.test

import okay.effects.{Runner, Std, Sum, Void}
import okay.effects.json.Json
import okay.effects.json.Json.{Str, num, obj}
import okay.effects.machine.Machine
import okay.effects.free.Free
import okay.effects.shape.{Problem, Shape}
import okay.effects.shape.Shape.{field, int, record, text}
import okay.faces.{Action, Chosen, Face, Faces, Navigation, Navigations, Pages, Rejected, Step, View}

/** THE STEP MACHINE AND THE TEST FACE, on an implementation: a scenario of three screens with a branch, a rejected
 * choice, back restoring a state handled inside, the views shown */
abstract class PlaysChecks(val fx0: Runner, name: String) extends munit.FunSuite {
  val faces: Faces[fx0.type] = new Faces[fx0.type]()(fx0)
  val plays: Plays[fx0.type] = new Plays[fx0.type]()(fx0)
  import fx0._
  import fx0.std._
  import faces._
  import plays._

  type F[X] = Sum[Void, Face, X]
  final case class Who(name: String, age: Int)
  val who: Shape[Who] = record(field("name", text), field("age", int))(Who(_, _))(w => (w.name, w.age))

  /** a greeting: a form, a confirmation, a branch on it, a word at the end */
  def greeting: String ! F =
    fill[Who, F](View.heading("Who are you?"), who).flatMap { w =>
      confirm[F](View.text(s"Greet ${w.name}, ${w.age}?")).flatMap { yes =>
        if (yes) show[F](View.text(s"Hello, ${w.name}!")).map(_ => s"greeted ${w.name}")
        else choose[String, F](View.text("Then what?"), List("Try again" -> "again", "Leave" -> "left")).flatMap {
          case "again" => greeting
          case other => pure[String, F](other)
        }
      }
    }

  /** the views a script shows, for the law across implementations (the implicits live in this class's scope) */
  def viewsFor(script: List[Chosen]): List[View] = play(steps[String, F, Void](greeting), Script.of(script: _*)).views

  test(s"$name: a scenario's steps — the views shown, the actions offered, the value at the end") {
    val ann = Chosen("submit", obj("name" -> Str("Ann"), "age" -> num(41)))
    val played = play(steps[String, F, Void](greeting), Script.of(ann, Chosen.plain("yes")))
    assertEquals(played, Played.Finished(List(View.heading("Who are you?"), View.text("Greet Ann, 41?"), View.text("Hello, Ann!")), "greeted Ann"))
    val left = play(steps[String, F, Void](greeting), Script.of(ann, Chosen.plain("no"), Chosen.plain("leave")))
    assertEquals(left, Played.Finished(List(View.heading("Who are you?"), View.text("Greet Ann, 41?"), View.text("Then what?")), "left"))
    val again = play(steps[String, F, Void](greeting), Script.of(ann, Chosen.plain("no"), Chosen.plain("try-again"), ann, Chosen.plain("yes")))
    assertEquals(again.views.size, 6)
    assertEquals(again, Played.Finished(again.views, "greeted Ann"))
  }
  test(s"$name: the actions of a step, and a rejected choice leaves the machine on it") {
    val stuck = play(steps[String, F, Void](greeting), Script.of(Chosen("submit", obj("name" -> Str("Ann")))))
    stuck match {
      case Played.Stuck(views, at, Some(Rejected(problems))) =>
        assertEquals(views, List(View.heading("Who are you?")))
        assertEquals(at.actions.map(_.name), List("submit"))
        assertEquals(problems, List(Problem(List("age"), "required")))
      case other => fail(s"not stuck: $other")
    }
    play(steps[String, F, Void](greeting), Script.of(Chosen.plain("nope"))) match {
      case Played.Stuck(_, _, Some(Rejected(problems))) => assertEquals(problems, List(Problem(List("action"), "one of submit")))
      case other => fail(s"not stuck: $other")
    }
  }
  test(s"$name: back re-resumes the previous step, and a state handled INSIDE Face comes back with it") {
    type FS[X] = Sum[F, ({ type L[Y] = State[Int, Y] })#L, X]
    /** a counter the person increments; the count is a State handled inside Face */
    def counting: Int ! FS = get[Int, FS].flatMap { n =>
      offer[Boolean, FS](View.text(s"count $n"), List(Action.plain[Boolean]("more", "More")(true), Action.plain[Boolean]("done", "Done")(false))).flatMap { more =>
        if (more) put[Int, FS](n + 1).flatMap(_ => counting) else pure[Int, FS](n)
      }
    }
    val inside: Int ! F = state(0)(counting).map(_._2)
    val forward = play(steps[Int, F, Void](inside), Script.of(Chosen.plain("more"), Chosen.plain("more"), Chosen.plain("done")))
    assertEquals(forward, Played.Finished(List(View.text("count 0"), View.text("count 1"), View.text("count 2")), 2))
    val undone = play(steps[Int, F, Void](inside), Script(Pick(Chosen.plain("more")), Pick(Chosen.plain("more")), Back, Pick(Chosen.plain("done"))))
    assertEquals(undone, Played.Finished(List(View.text("count 0"), View.text("count 1"), View.text("count 2"), View.text("count 1")), 1))
    val tooFar = play(steps[Int, F, Void](inside), Script(Back, Pick(Chosen.plain("done"))))
    assertEquals(tooFar, Played.Finished(List(View.text("count 0"), View.text("count 0")), 0))
  }
  test(s"$name: choose names its actions from the labels, uniquely") {
    val p: String ! F = choose[String, F](View.text("?"), List("A b" -> "1", "a-b" -> "2", "!!" -> "3"))
    play(steps[String, F, Void](p), Script.of()) match {
      case Played.Stuck(_, at, None) => assertEquals(at.actions.map(_.name), List("a-b", "a-b-2", "x"))
      case other => fail(s"$other")
    }
  }
}

class NavigationSuite extends munit.FunSuite {
  val nav = new Navigations[Machine.type]()(Machine)
  import Machine._
  import nav._
  sealed trait Page; case object Home extends Page; final case class User(id: Int) extends Page
  type N[X] = Navigation[Page, X]
  type R[X] = Sum[Void, N, X]
  def visit: Page ! R = push[Page, R](User(1)).flatMap(_ => push[Page, R](User(2))).flatMap(_ => replace[Page, R](User(3))).flatMap(_ => current[Page, R])

  test("recording: the pages asked for; memory: the pages kept, the face told, back") {
    assertEquals(run(recording[Page](Home)(visit)), (List(Home, User(1), User(3)), User(3): Page))
    val pages = new Pages[Page](Home)
    val seen = scala.collection.mutable.ListBuffer.empty[Page]
    pages.onChange(seen += _)
    assertEquals(run(memory[Page](pages)(visit)), User(3): Page)
    assertEquals(pages.history, List(User(3), User(1), Home))
    assertEquals(seen.toList, List(User(1), User(2), User(3)))
    assert(pages.back()); assertEquals(pages.current, User(1): Page)
    assert(pages.back()); assert(!pages.back()); assertEquals(pages.current, Home: Page)
  }
}

class PlaysOnMachine extends PlaysChecks(Machine, "machine")
class PlaysOnFree extends PlaysChecks(Free, "free")

/** the law: the same script, the same views, on both implementations */
class SameViewsSuite extends munit.FunSuite {
  test("machine and Free show the same views for the same script") {
    val m = new PlaysOnMachine; val f = new PlaysOnFree
    val script = List(Chosen("submit", obj("name" -> Str("Bo"), "age" -> num(7))), Chosen.plain("no"), Chosen.plain("leave"))
    assertEquals(m.viewsFor(script), f.viewsFor(script))
    assertEquals(m.viewsFor(script).size, 3)
  }
}
