package okay

import okay.effects.Runner
import okay.effects.tagless.Tagless

/**
 * `import okay.faces.*`: EVERYTHING A PROGRAM OF A FACE IS WRITTEN WITH, and nothing of any implementation — the faces'
 * words over tagless (`offer`, `show`, `confirm`, `choose`, `fill`, `fillOrCancel`) and tagless's own (`!`,
 * `Pure`, the rows, `flatMap`, `map`, `into`, `pure`), so a program is a value: `val games: Outcome ! Face = …`.
 * `run(p)` runs such a program of nothing else on the implementation in scope: `import
 * okay.effects.machine.Machine.self`.
 */
package object faces extends okay.faces.Faces[Tagless.type]()(Tagless):
  export Tagless.{
    `!`, Pure, Row, `+`, `*`, `%`, In, Split, Sub, pure, flatMap, map, into, splitOne, splitLast, splitInit, subPure,
    subOne, subMore
  }
  def run[A](p: A ! Pure)(implicit fx: Runner): A = Tagless.run[A](p)(using fx)
