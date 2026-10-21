package okay.face.examples

import okay.effects.Effects
import okay.effects.shape.Shape
import okay.face.{Action, Face, Faces, Look, Marks, Rule, Scenario, Style, Theme, Tone, View}

/** NOUGHTS AND CROSSES: the person is X, the program O. A loop of offers — the board shown, the free cells the one
 * action's choices — branching on the outcome, a `Tell` at the end; back is the face's and undoes a move. The same
 * program plays in a terminal, over MCP against an agent, and in a test by script */
class Noughts[F <: Effects with Singleton](implicit val faces: Faces[F]) extends Scenario[F, Outcome] {
  import faces.fx._
  import faces.{offer, show}

  /** THE GAME'S OWN MARKS, typed: the result line is marked `result`; a style may dress it, an agent is told what it is */
  sealed trait Mark
  case object Result extends Mark
  case object Cell extends Mark
  val marks: Marks[Mark] = Marks.of(Shape.derived[Mark]).explained(Result -> "how the game ended: who won, or a draw", Cell -> "a cell of the board: X, O, or free to play")
  /** the look: a cell big enough to tap, bordered; the result bold */
  val style: Style = Style.default.set(marks.role(Cell), Rule(size = 1.6, pad = 1, border = Some(Tone.Border), flow = okay.face.Flow.Row, centered = true))
    .set(marks.role(Result), Rule(bold = true, color = Some(Tone.Accent), centered = true))
    .adjust(okay.face.Role.Heading(1))(_.copy(centered = true)).adjust(okay.face.Role.Table)(_.copy(centered = true)).adjust(okay.face.Role.Actions)(_.copy(centered = true))
  val light: Look = Look(Theme.light, style)
  val dark: Look = Look(Theme.dark, style)

  /** the rules, where a face explains an action: the terminal's `?`, an agent's tool description, a title on the web */
  val rules: String = "you are X, the program is O. A free cell is a [number]: choose one and X goes there, then O answers. " +
    "Three in a row — across, down or diagonally — wins; a full board with no row is a draw"
  /** the board: a taken cell its mark, a free one a `Ref` by its number — live, since `ref` is offered: a face that can
   * click plays by a click; the terminal and an agent give the number */
  def picture(board: Board): View = View.stack(View.heading("Noughts and crosses"),
    View.Table(Vector("", "", ""), board.cells.zipWithIndex.map { case (c, i) => marks.mark(Cell, c.fold[View](View.Ref((i + 1).toString, "·"))(x => View.Text(x.toString))) }.grouped(3).toVector))
  def ended(board: Board, o: Outcome): View = View.stack(picture(board), marks.mark(Result, View.text(o match { case Won(c) => s"$c won"; case Draw => "a draw" })))

  /** one game: the outcome, once the end is seen and "Play again" taken */
  def game[R[_]](board: Board)(implicit in: In[Face, R]): Outcome ! R = board.outcome match {
    case Some(o) => offer[Unit, R](ended(board, o), List(Action.plain[Unit]("again", "Play again", Some("a new game, the board clear; you are X and begin"))(()))).map(_ => o)
    case None =>
      // the one move is `ref`: a free cell clicked where a face clicks, its number given where one types or calls
      val cells = Shape.choice[Int](board.free.map(i => (i + 1).toString -> i): _*).described("Cell", Some("1 to 9, left to right, top to bottom"))
      offer[Int, R](picture(board), List(Action[Int, Int]("ref", "Play", Shape.record(Shape.field("key", cells))(c => c)(c => c), Some(rules))(c => c)))
        .flatMap(i => game[R](board.played(i, 'X').replied))
  }
  /** games without end, one after another: what a window or a page runs */
  def games[R[_]](implicit in: In[Face, R]): Outcome ! R = game[R](Board.empty).flatMap(_ => games[R])
  /** as a scenario: the games, titled and dressed, no back — `SwingFace.handle(game)`, `Terminal.handle(game)` */
  def program[R[_]](implicit in: In[Face, R]): Outcome ! R = games[R]
  override def title: String = "Noughts and crosses"
  override def look: Option[Look] = Some(light)
  override def back: Boolean = false
  override def vocabulary: okay.face.Vocabulary = marks.vocabulary
  override def instructions: Option[String] = Some(rules)
}
