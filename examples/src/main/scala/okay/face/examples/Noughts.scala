package okay.face.examples

import okay.effects.shape.Shape
import okay.face.{Action, Look, Marks, Rule, Style, Theme, Tone, View}

/** the game's own types, beside the class: the rules with no effect in them, and how a game ends */
object Noughts {
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

  /** THE GAME'S OWN MARKS, typed: the result line is marked `result`; a style may dress it, an agent is told what it is */
  sealed trait Mark
  case object Result extends Mark
  case object Cell extends Mark
  val marks: Marks[Mark] = Marks.of(Shape.derived[Mark]).explained(Result -> "how the game ended: who won, or a draw", Cell -> "a cell of the board: X, O, or free to play")
  /** the look: a cell big enough to tap, bordered; the result bold; the board in the middle */
  val style: Style = Style.default.set(marks.role(Cell), Rule(size = 1.6, pad = 1, border = Some(Tone.Border), flow = okay.face.Flow.Row, centered = true))
    .set(marks.role(Result), Rule(bold = true, color = Some(Tone.Accent), centered = true))
    .adjust(okay.face.Role.Heading(1))(_.copy(centered = true)).adjust(okay.face.Role.Table)(_.copy(centered = true)).adjust(okay.face.Role.Actions)(_.copy(centered = true))
  val light: Look = Look(Theme.light, style)
  val dark: Look = Look(Theme.dark, style)

  /** the rules: shown first, so a face has them to tell — the terminal prints them, an agent gets them as instructions */
  val rules: View = View.text("You are X, the program is O. A free cell is a [number]: choose one and X goes there, then O answers. " +
    "Three in a row — across, down or diagonally — wins; a full board with no row is a draw.")
  /** the board: a taken cell its mark, a free one a `Ref` by its number — live, since `ref` is offered: a face that can
   * click plays by a click; the terminal and an agent give the number */
  def picture(board: Board): View = View.stack(View.heading("Noughts and crosses"),
    View.Table(Vector("", "", ""), board.cells.zipWithIndex.map { case (c, i) => marks.mark(Cell, c.fold[View](View.Ref((i + 1).toString, "·"))(x => View.Text(x.toString))) }.grouped(3).toVector))
  def ended(board: Board, o: Outcome): View = View.stack(picture(board), marks.mark(Result, View.text(o match { case Won(c) => s"$c won"; case Draw => "a draw" })))

  /** THE PROGRAMS: values, written with `okay.face`'s words alone — the faces' over tagless, so a program is of no
   * implementation, and every face instantiates it on its own. `game` one game, `games` games without end; each
   * `A ! Only`, `Face` alone in the row. The rules are told first, so a face has them to tell */
  import okay.face._
  val game: Outcome ! Only = show[Only](rules).flatMap(_ => play(Board.empty))
  val games: Outcome ! Only = { def loop: Outcome ! Only = play(Board.empty).flatMap(_ => loop); show[Only](rules).flatMap(_ => loop) }
  /** one game from the board given: a loop of offers — the board shown, the free cells the one move, a cell by its number —
   * branching on the outcome, "Play again" at the end; no step goes back, a game is its own rules */
  private def play(board: Board): Outcome ! Only = board.outcome match {
    case Some(o) => offer[Unit, Only](ended(board, o), List(Action.plain[Unit]("again", "Play again", Some("a new game, the board clear; you are X and begin"))(())), back = false).map(_ => o)
    case None =>
      // the one move is `ref`: a free cell clicked where a face clicks, its number given where one types or calls
      val cells = Shape.choice[Int](board.free.map(i => (i + 1).toString -> i): _*).described("Cell", Some("1 to 9, left to right, top to bottom"))
      offer[Int, Only](picture(board), List(Action[Int, Int]("ref", "Play", Shape.record(Shape.field("key", cells))(c => c)(c => c), Some("a free cell: X goes there, then O answers"))(c => c)), back = false)
        .flatMap(i => play(board.played(i, 'X').replied))
  }
}
