package okay.face

import okay.effects.Effects

/**
 * A JUMP FROM ANY SCREEN: a global menu, "home" from deep inside, a sign-out — a place named from anywhere, the
 * screens unwound, the one named begun. `jump(place)` is an `Error[P]` raised; `jumping(start)(screen)` is the loop
 * that attempts each screen and, on a jump, begins at the place jumped to. Unwinding is the standard error's: the
 * handlers inside `Face` lose their state with the screens, the world's outside keep theirs — as a face's back does.
 * DECIDED over a history jump by the face: a jump is the program's own, so every face has it the same way.
 */
class Jumps[F <: Effects with Singleton](implicit implementation: F) {
  val fx: F = implementation
  import fx._
  import fx.std.{Error, attempt, raise}
  type Jump[P, X] = Error[P, X]

  def jump[P, R[_], X](to: P)(implicit in: In[({ type L[x] = Error[P, x] })#L, R]): X ! R = raise[P, R, X](to)
  /** the screens from `start`, each attempted; a jump begins the screen of the place jumped to; done when a screen ends without one */
  def jumping[P, R[_], Rest[_]](start: P)(screen: P => Unit ! R)(implicit sp: Split[({ type L[x] = Error[P, x] })#L, R, Rest]): Unit ! Rest = {
    def loop(p: P): Unit ! Rest = attempt[P](screen(p)).flatMap { case Left(next) => loop(next); case Right(()) => pure[Unit, Rest](()) }
    loop(start)
  }
}
object Jumps {
  def apply()(implicit fx: Effects): Jumps[fx.type] = { implicit val exact: fx.type = fx; new Jumps[fx.type] }
}
