package okay.faces.examples

import okay.effects.json.Json.{Str, obj}
import okay.effects.machine.Machine._
import okay.effects.shape.Problem
import okay.faces.{Chosen, Rejected, View}
import okay.faces.examples.Noughts._
import okay.faces.test.Plays

/** THE GAME BY SCRIPT: the test face handles the program; what it shows, what it refuses, how it ends */
class NoughtsSuite extends munit.FunSuite {
  val plays = Plays()
  import plays._
  def cell(n: Int): Chosen = Chosen("ref", obj("key" -> Str(n.toString)))

  test("X wins down the left: the rules told, three boards, the end; Play again ends the game with the outcome") {
    plays.handle(Noughts.game)(Script.of(cell(1), cell(4), cell(7), Chosen.plain("again"))) match {
      case Played.Finished(views, Won('X')) => assertEquals(views.size, 5); assertEquals(views.head, rules)
      case other => fail(s"$other")
    }
  }
  test("a taken cell is not among the choices: rejected, the board unchanged; the one action is the move") {
    plays.handle(Noughts.game)(Script.of(cell(1), cell(1))) match {
      case Played.Stuck(views, at, Some(Rejected(problems))) =>
        assertEquals(views.size, 3)
        assertEquals(problems, List(Problem(List("key"), "one of 2, 3, 4, 6, 7, 8, 9")))
        assertEquals(at.actions.map(_.name), List("ref")); assertEquals(at.back, false)
      case other => fail(s"$other")
    }
  }
  test("no step may be returned to: none is kept in history, Back leaves the step as it is") {
    plays.handle(Noughts.game)(Script(Pick(cell(1)), Back)) match {
      case Played.Stuck(views, at, None) => assertEquals(views.size, 4); assertEquals(views(2), views(3))
      case other => fail(s"$other")
    }
  }
  test("a draw: every cell filled, nobody won") {
    plays.handle(Noughts.game)(Script.of(cell(1), cell(9), cell(8), cell(4), cell(7), Chosen.plain("again"))) match {
      case Played.Finished(_, o) => assert(o == Draw || o.isInstanceOf[Won], s"$o")
      case other => fail(s"$other")
    }
  }
}

/** the same game at the terminal, under a scripted console: the rules printed, a cell by its key; a quit is None */
class NoughtsTerminalSuite extends munit.FunSuite {
  import okay.effects.machine.Machine._
  val io = new okay.effects.world.Io()
  val term = okay.faces.term.Terminal()
  test("X wins down the left at the terminal: the boards printed, a cell by its key, the outcome once played again") {
    val played = term.handle(Noughts.game)
    val (printed, result) = run(io.console(List("1", "4", "7", "again"))(played))
    assertEquals(result, Some(Won('X')))
    assert(printed.contains("NOUGHTS AND CROSSES"))
    assert(printed.exists(_.contains("You are X, the program is O")), printed.mkString("\n"))
    assert(printed.contains("X won"), printed.mkString("\n"))
    assert(printed.exists(_.contains("a [key]")) && !printed.exists(_.contains("< back")), printed.mkString("\n"))
  }
}
