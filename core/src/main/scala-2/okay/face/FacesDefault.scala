package okay.face

import okay.effects.Effects

/** Scala 2: `F` taken from the dependent result — the implicit implementation's own singleton type */
trait FacesDefault {
  implicit def default(implicit fx: Effects): Faces[fx.type] = { implicit val exact: fx.type = fx; new Faces[fx.type] }
}
