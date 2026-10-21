package okay.face

import okay.effects.{Effects, Sum}

/**
 * A SCENARIO: a program in `Face` and whatever else, over any row that has `Face` — the thing a face runs, with what a
 * face needs to know about it: a title for a window or a page, a look, whether it offers going back. A class of the
 * application extends it over the faces in scope: `class Noughts(implicit val faces: Faces[F]) extends Scenario[F, Outcome]`,
 * and then a face takes it whole — `run(SwingFace.handle(game))`, `run(io.realConsole(Terminal.handle(game)))`.
 */
trait Scenario[F <: Effects with Singleton, A] {
  val faces: Faces[F]
  /** the program, over any row that has `Face` */
  def program[R[_]](implicit in: faces.fx.In[Face, R]): faces.fx.![A, R]
  def title: String = "okay"
  def look: Option[Look] = None
  /** whether the dialogue offers going back; a game of its own rules says no */
  def back: Boolean = true
  /** what an agent is told at the start, beside the tools */
  def instructions: Option[String] = None
  /** the application's marks on views, told to an agent and served by REST */
  def vocabulary: Vocabulary = Vocabulary.empty

  /** `Face` alone in the row: what a face of one person runs */
  type Only[X] = Sum[faces.fx.Pure, Face, X]
  /** the program's steps with `Face` handled and nothing left in the row */
  final def steps: faces.fx.![Step[A, ({ type L[X] = faces.fx.![X, faces.fx.Pure] })#L], faces.fx.Pure] = {
    import faces.fx._
    faces.steps[A, Only, Pure](program[Only])
  }
}
