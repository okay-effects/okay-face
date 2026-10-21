package okay.face.examples

import okay.effects.Effects
import okay.effects.shape.Shape
import okay.face.{Action, Face, Faces, Marks, View}

/** NOUGHTS AND CROSSES: the person is X, the program O. A loop of offers — the board shown, the free cells the one
 * action's choices — branching on the outcome, a `Tell` at the end; back is the face's and undoes a move. The same
 * program plays in a terminal, over MCP against an agent, and in a test by script */
class Noughts[F <: Effects](val faces: Faces[F]) {
  import faces.fx._
  import faces.{offer, show}

  final case class Board(cells: Vector[Option[Char]]) {
    def free: List[Int] = cells.zipWithIndex.collect { case (None, i) => i }.toList
    def played(i: Int, c: Char): Board = Board(cells.updated(i, Some(c)))
    private val lines = List(List(0, 1, 2), List(3, 4, 5), List(6, 7, 8), List(0, 3, 6), List(1, 4, 7), List(2, 5, 8), List(0, 4, 8), List(2, 4, 6))
    def winner: Option[Char] = lines.collectFirst { case l if cells(l.head).isDefined && l.forall(i => cells(i) == cells(l.head)) => cells(l.head).get }
    def outcome: Option[Outcome] = winner.map(Won(_)).orElse(if (free.isEmpty) Some(Draw) else None)
    /** the program's reply: the first free cell, centre first — not clever, and not the point */
    def replied: Board = if (outcome.isDefined) this else { val i = if (cells(4).isEmpty) 4 else free.head; played(i, 'O') }
  }
  object Board { val empty: Board = Board(Vector.fill(9)(None)) }
  sealed trait Outcome
  final case class Won(by: Char) extends Outcome
  case object Draw extends Outcome
  sealed trait Move
  final case class Play(cell: Int) extends Move
  case object Restart extends Move
  /** THE GAME'S OWN MARKS, typed: the result line is marked `result`; a style may dress it, an agent is told what it is */
  sealed trait Mark
  case object Result extends Mark
  val marks: Marks[Mark] = Marks.of(Shape.derived[Mark]).explained(Result -> "how the game ended: who won, or a draw")

  def picture(board: Board): View = View.stack(View.heading("Noughts and crosses"),
    View.Table(Vector("", "", ""), board.cells.map(c => View.Text(c.fold("·")(_.toString))).grouped(3).toVector))
  def ended(board: Board, o: Outcome): View = View.stack(picture(board), marks.mark(Result, View.text(o match { case Won(c) => s"$c won"; case Draw => "a draw" })))

  def game[R[_]](board: Board)(implicit in: In[Face, R]): Outcome ! R = board.outcome match {
    case Some(o) => show[R](ended(board, o)).map(_ => o)
    case None =>
      val cells = Shape.choice[Int](board.free.map(i => (i + 1).toString -> i): _*).described("Cell", Some("1 to 9, left to right, top to bottom"))
      offer[Move, R](picture(board), List(
        Action[Int, Move]("play", "Play", Shape.record(Shape.field("cell", cells))(c => c)(c => c))(Play(_)),
        Action.plain[Move]("restart", "Start over")(Restart)))
        .flatMap {
          case Play(i) => game[R](board.played(i, 'X').replied)
          case Restart => game[R](Board.empty)
        }
  }
}
