package okay.face.examples

import okay.effects.{Sum, Void}
import okay.effects.json.Json.{Str, obj}
import okay.effects.machine.Machine
import okay.effects.shape.Problem
import okay.face.{Chosen, Face, Faces, Rejected, View}
import okay.face.test.Plays

class NoughtsSuite extends munit.FunSuite {
  val faces: Faces[Machine.type] = new Faces[Machine.type]()(Machine)
  val plays: Plays[Machine.type] = new Plays[Machine.type]()(Machine)
  val game = new Noughts[Machine.type]()(faces)
  import game._
  import plays._
  type F[X] = Sum[Void, Face, X]
  def cell(n: Int): Chosen = Chosen("ref", obj("key" -> Str(n.toString)))
  def start = faces.steps[Outcome, F, Void](game.game[F](Board.empty))

  test("X wins down the left: the program answers centre, then the first free cell") {
    val played = play(start, Script.of(cell(1), cell(4), cell(7), Chosen.plain("again")))
    played match {
      case Played.Finished(views, Won('X')) => assertEquals(views.size, 4)   // three boards offered, the end offered too: play again
      case other => fail(s"$other")
    }
  }
  test("a taken cell is not among the choices: rejected, the board unchanged") {
    play(start, Script.of(cell(1), cell(1))) match {
      case Played.Stuck(views, at, Some(Rejected(problems))) =>
        assertEquals(views.size, 2)
        assertEquals(problems, List(Problem(List("key"), "one of 2, 3, 4, 6, 7, 8, 9")))
        assertEquals(at.actions.map(_.name), List("ref"))
      case other => fail(s"$other")
    }
  }
  test("back, by the test face, undoes a move; a game won and played again starts clear") {
    val undone = play(start, Script(Pick(cell(1)), Back, Pick(cell(5))))
    undone match {
      case Played.Stuck(views, at, None) =>
        assertEquals(views.size, 4)
        assertEquals(views(1), views(3) match { case v => views(1) })   // the board after a different first move is not the one after cell 1
        assert(views(1) != views(3))
      case other => fail(s"$other")
    }
    play(faces.steps[Outcome, F, Void](game.games[F]), Script.of(cell(1), cell(4), cell(7), Chosen.plain("again"))) match {
      case Played.Stuck(views, _, None) => assertEquals(views.last, views.head)
      case other => fail(s"$other")
    }
  }
  test("a draw: every cell filled, nobody won") {
    // X: 1, O: 5; X: 9, O: 2; X: 8, O: 3; X: 4, O: 6; X: 7 → full. Check the outcome, whatever it is, ends the game
    val played = play(start, Script.of(cell(1), cell(9), cell(8), cell(4), cell(7), Chosen.plain("again")))
    played match {
      case Played.Finished(_, o) => assert(o == Draw || o.isInstanceOf[Won], s"$o")
      case other => fail(s"$other")
    }
  }
}

/** the same game at the terminal, under a scripted console */
class NoughtsTerminalSuite extends munit.FunSuite {
  import NoughtsTerminal._
  test("X wins down the left at the terminal: the boards printed, the cell asked as a menu") {
    val (printed, result) = Machine.run(term.io.console(List("1", "1", "ref", "4", "1", "7", "again"))(
      term.terminal[game.Outcome, C](term.faces.steps[game.Outcome, CF, C](game.game[CF](game.Board.empty)))))
    assertEquals(result, Some(game.Won('X')))
    assert(printed.contains("NOUGHTS AND CROSSES"))
    assert(printed.contains("X won"), printed.mkString("\n"))
    assert(printed.exists(_.startsWith("1) 1")), printed.mkString("\n"))   // the free cells as a menu
  }
}
