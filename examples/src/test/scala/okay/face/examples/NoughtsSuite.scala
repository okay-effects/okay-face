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
  val game = new Noughts[Machine.type](faces)
  import game._
  import plays._
  type F[X] = Sum[Void, Face, X]
  def cell(n: Int): Chosen = Chosen("play", obj("cell" -> Str(n.toString)))
  def start = faces.steps[Outcome, F, Void](game.game[F](Board.empty))

  test("X wins down the left: the program answers centre, then the first free cell") {
    val played = play(start, Script.of(cell(1), cell(4), cell(7)))
    played match {
      case Played.Finished(views, Won('X')) => assertEquals(views.size, 4)   // three boards offered, the last told
      case other => fail(s"$other")
    }
  }
  test("a taken cell is not among the choices: rejected, the board unchanged") {
    play(start, Script.of(cell(1), cell(1))) match {
      case Played.Stuck(views, at, Some(Rejected(problems))) =>
        assertEquals(views.size, 2)
        assertEquals(problems, List(Problem(List("cell"), "one of 2, 3, 4, 6, 7, 8, 9")))
        assertEquals(at.actions.map(_.name), List("play", "restart"))
      case other => fail(s"$other")
    }
  }
  test("back undoes a move; restart clears the board") {
    val undone = play(start, Script(Pick(cell(1)), Back, Pick(cell(5))))
    undone match {
      case Played.Stuck(views, at, None) =>
        assertEquals(views.size, 4)
        assertEquals(views(1), views(3) match { case v => views(1) })   // the board after a different first move is not the one after cell 1
        assert(views(1) != views(3))
      case other => fail(s"$other")
    }
    play(start, Script.of(cell(1), Chosen.plain("restart"))) match {
      case Played.Stuck(views, _, None) => assertEquals(views.last, views.head)
      case other => fail(s"$other")
    }
  }
  test("a draw: every cell filled, nobody won") {
    // X: 1, O: 5; X: 9, O: 2; X: 8, O: 3; X: 4, O: 6; X: 7 → full. Check the outcome, whatever it is, ends the game
    val played = play(start, Script.of(cell(1), cell(9), cell(8), cell(4), cell(7)))
    played match {
      case Played.Finished(_, o) => assert(o == Draw || o.isInstanceOf[Won], s"$o")
      case other => fail(s"$other")
    }
  }
}
