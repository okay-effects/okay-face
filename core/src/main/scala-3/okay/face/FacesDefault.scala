package okay.face

import okay.effects.Effects

/** Scala 3: `F` inferred from the implicit implementation, kept exact by the takers' `Singleton` bound */
trait FacesDefault:
  implicit def default[F <: Effects with Singleton](implicit fx: F): Faces[F] = new Faces[F]()(fx)
