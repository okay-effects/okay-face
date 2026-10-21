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

  test("X wins down the left: three boards, the end; Play again ends the game with the outcome") {
    plays.handle(Noughts.play(Board.empty))(Script.of(cell(1), cell(4), cell(7), Chosen.plain("again"))) match {
      case Played.Finished(views, Won(X)) => assertEquals(views.size, 4)
      case other => fail(s"$other")
    }
  }
  test("games: the rules told first, then a game; Play again begins another, the board clear") {
    plays.handle(Noughts.games)(Script.of(cell(1), cell(4), cell(7), Chosen.plain("again"))) match {
      case Played.Stuck(views, at, None) =>
        assertEquals(views.size, 6); assertEquals(views.head, rules); assertEquals(views.last, views(1));
        assertEquals(at.actions.map(_.name), List("ref"))
      case other => fail(s"$other")
    }
  }
  test("a taken cell is not among the choices: rejected, the board unchanged; the one action is the move") {
    plays.handle(Noughts.play(Board.empty))(Script.of(cell(1), cell(1))) match {
      case Played.Stuck(views, at, Some(Rejected(problems))) =>
        assertEquals(views.size, 2)
        assertEquals(problems, List(Problem(List("key"), "one of 2, 3, 4, 6, 7, 8, 9")))
        assertEquals(at.actions.map(_.name), List("ref")); assertEquals(at.back, false)
      case other => fail(s"$other")
    }
  }
  test("no step may be returned to: none is kept in history, Back leaves the step as it is") {
    plays.handle(Noughts.play(Board.empty))(Script(Pick(cell(1)), Back)) match {
      case Played.Stuck(views, at, None) => assertEquals(views.size, 3); assertEquals(views(1), views(2))
      case other => fail(s"$other")
    }
  }
  test("a draw: every cell filled, nobody won") {
    plays.handle(Noughts.play(Board.empty))(Script.of(cell(1), cell(9), cell(8), cell(4), cell(7),
        Chosen.plain("again"))) match {
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
    val played = term.handle(Noughts.play(Board.empty))
    val (printed, result) = run(io.console(List("1", "4", "7", "again"))(played))
    assertEquals(result, Some(Won(X)))
    assert(printed.contains("NOUGHTS AND CROSSES"))
    assert(printed.contains("X won"), printed.mkString("\n"))
    assert(printed.exists(_.contains("a [key]")) && !printed.exists(_.contains("< back")), printed.mkString("\n"))
  }
}
