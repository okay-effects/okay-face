package okay.face.examples

/** THE RULES, no effect in them: a board of nine cells, X and O, the lines that win, the program's reply */
final case class Board(cells: Vector[Option[Char]]) {
  def free: List[Int] = cells.zipWithIndex.collect { case (None, i) => i }.toList
  def played(i: Int, c: Char): Board = Board(cells.updated(i, Some(c)))
  def winner: Option[Char] = Board.lines.collectFirst { case l if cells(l.head).isDefined && l.forall(i => cells(i) == cells(l.head)) => cells(l.head).get }
  def outcome: Option[Outcome] = winner.map(Won(_)).orElse(if (free.isEmpty) Some(Draw) else None)
  /** the program's reply: the first free cell, centre first — not clever, and not the point */
  def replied: Board = if (outcome.isDefined) this else { val i = if (cells(4).isEmpty) 4 else free.head; played(i, 'O') }
}
object Board {
  val empty: Board = Board(Vector.fill(9)(None))
  val lines: List[List[Int]] = List(List(0, 1, 2), List(3, 4, 5), List(6, 7, 8), List(0, 3, 6), List(1, 4, 7), List(2, 5, 8), List(0, 4, 8), List(2, 4, 6))
}

/** how a game ends */
sealed trait Outcome
final case class Won(by: Char) extends Outcome
case object Draw extends Outcome
