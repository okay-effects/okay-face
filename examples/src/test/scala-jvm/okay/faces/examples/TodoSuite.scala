package okay.faces.examples

import okay.effects.json.Json.{Str, obj}
import okay.effects.machine.Machine._
import okay.effects.shape.{Day, Problem}
import okay.faces.examples.Todo._
import okay.faces.test.Plays
import okay.faces.{Chosen, Rejected, View}

/** THE LIST BY SCRIPT on the test face: a form filled, an item done by its number, one removed after a choice and a
 * yes, the done ones cleared, back as undo; what the program shows and how it ends — the reference every face agrees
 * with */
class TodoSuite extends munit.FunSuite {
  val plays = Plays()
  import plays.{Back, Pick, Played, Script}
  def add(title: String, due: Option[String] = None): Chosen =
    Chosen("add", obj(("title" -> Str(title)) :: due.toList.map(d => "due" -> Str(d)): _*))
  def item(n: Int): Chosen = Chosen("ref", obj("key" -> Str(n.toString)))

  test("two added, one done, then left: the list kept, what was told and shown in order") {
    plays.handle(Todo.keeping)(Script.of(add("milk"), add("bread", Some("2026-10-25")), item(1),
        Chosen.plain("leave"))) match {
      case Played.Finished(views, t) =>
        assertEquals(t.items.map(_._2), Vector(Item("milk"), Item("bread", Some(Day(2026, 10, 25)))))
        assertEquals(t.done, Set(1))
        assertEquals(views.head, about)
        assertEquals(views(2), View.info("added: milk"))
        assert(views.last.toString.contains("1 of 2 done"), views.last.toString)
      case other => fail(s"$other")
    }
  }
  test("the form checks: no title, the step unchanged, the problem at the field; a due day that is not a day") {
    plays.handle(Todo.keeping)(Script.of(Chosen("add", obj()))) match {
      case Played.Stuck(_, at, Some(Rejected(problems))) =>
        assertEquals(problems.map(_.path), List(List("title")));
        assertEquals(at.actions.map(_.name), List("add", "leave"))
      case other => fail(s"$other")
    }
    plays.handle(Todo.keeping)(Script.of(add("x", Some("soon")))) match {
      case Played.Stuck(_, _, Some(Rejected(problems))) => assertEquals(problems.map(_.path), List(List("due")))
      case other => fail(s"$other")
    }
  }
  test("remove one: a choice among the items by title, a yes; no undoes nothing") {
    val script = Script.of(add("milk"), add("bread"), Chosen.plain("remove"), Chosen.plain("bread"),
      Chosen.plain("yes"),
      Chosen.plain("remove"), Chosen.plain("milk"), Chosen.plain("no"), Chosen.plain("leave"))
    plays.handle(Todo.keeping)(script) match {
      case Played.Finished(views, t) =>
        assertEquals(t.items.map(_._2.title), Vector("milk"))
        assert(views.exists(_ == View.text("Remove which?")) && views.exists(_ == View.text("Remove 'bread'?")),
          views.toString)
      case other => fail(s"$other")
    }
  }
  test("clear the done: offered only once something is done; a yes removes them all") {
    plays.handle(Todo.keeping)(Script.of(add("a"), add("b"), item(1), Chosen.plain("clear"), Chosen.plain("yes"),
        Chosen.plain("leave"))) match {
      case Played.Finished(_, t) =>
        assertEquals(t.items.map(_._2.title), Vector("b")); assertEquals(t.done, Set.empty[Int])
      case other => fail(s"$other")
    }
    plays.handle(Todo.keeping)(Script.of(add("a"), Chosen.plain("clear"))) match {
      case Played.Stuck(_, at, Some(Rejected(List(Problem(List("action"), _))))) =>
        assertEquals(at.actions.map(_.name), List("add", "ref", "remove", "leave"))
      case other => fail(s"$other")
    }
  }
  test("back is undo: the list as it was before the last step") {
    plays.handle(Todo.keeping)(Script(Pick(add("a")), Pick(add("b")), Back, Pick(Chosen.plain("leave")))) match {
      case Played.Finished(_, t) => assertEquals(t.items.map(_._2.title), Vector("a"))
      case other => fail(s"$other")
    }
  }
}
