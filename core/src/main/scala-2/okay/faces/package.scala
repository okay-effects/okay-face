package okay

import okay.effects.Runner
import okay.effects.tagless.Tagless

/**
 * `import okay.faces._`: EVERYTHING A PROGRAM OF A FACE IS WRITTEN WITH, and nothing of any implementation — the faces'
 * words over tagless (`offer`, `show`, `confirm`, `choose`, `fill`, `fillOrCancel`) and tagless's own (`!`,
 * `Pure`, the rows by names, `flatMap`, `map`, `into`, `pure`), so a program is a value: `val games: Outcome ! Face =
 * …`.
 * `run(p)` runs such a program of nothing else on the implementation in scope: `import
 * okay.effects.machine.Machine.self`.
 */
package object faces extends okay.faces.Faces[Tagless.type]()(Tagless) {
  type ![+A, R[_]] = Tagless.![A, R]
  type Pure[X] = Tagless.Pure[X]
  type Row[N <: okay.effects.Effect, X] = Tagless.Row[N, X]
  type +[R <: okay.effects.Effect, E <: okay.effects.Effect] = Tagless.+[R, E]
  type *[F[_]] = Tagless.*[F]
  type %[F[_, _], S] = Tagless.%[F, S]
  type In[E[_], R[_]] = Tagless.In[E, R]
  type Split[E[_], R[_], Rest[_]] = Tagless.Split[E, R, Rest]
  type Sub[R[_], S[_]] = Tagless.Sub[R, S]
  // the evidence, found as tagless finds it
  implicit def splitOne[E[_]]: Split[E, E, Pure] = Tagless.splitOne[E]
  implicit def splitLast[R[_], E[_]]: Split[E, ({ type L[x] = okay.effects.Sum[R, E, x] })#L, R] =
    Tagless.splitLast[R, E]
  implicit def splitInit[E[_], R[_], G[_], Rest[_]](implicit s: Split[E, R, Rest])
    : Split[E, ({ type L[x] = okay.effects.Sum[R, G, x] })#L, ({ type L[x] = okay.effects.Sum[Rest, G, x] })#L] =
    Tagless.splitInit[E, R, G, Rest](s)
  implicit def subPure[S[_]]: Sub[Pure, S] = Tagless.subPure[S]
  implicit def subOne[E[_], S[_]](implicit e: In[E, S]): Sub[E, S] = Tagless.subOne[E, S](e)
  implicit def subMore[R[_], E[_], S[_]](implicit r: Sub[R, S], e: In[E, S])
    : Sub[({ type L[x] = okay.effects.Sum[R, E, x] })#L, S] = Tagless.subMore[R, E, S](r, e)
  def pure[A, R[_]](a: A): A ! R = Tagless.pure[A, R](a)
  implicit def programOps[A, R[_]](p: A ! R): Tagless.ProgramOps[A, R] = new Tagless.ProgramOps[A, R](p)
  def run[A](p: A ! Pure)(implicit fx: Runner): A = Tagless.run[A](p)(fx)
}
