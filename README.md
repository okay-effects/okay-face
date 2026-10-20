# okay-face — an application's capabilities as effects, its faces as handlers

An application has several faces: a screen for a person, tools for an agent, resources for a program. Each shows WHAT
IS THERE and WHAT CAN BE DONE. okay-face writes that once, as a program over an algebraic effect, and each face is a
handler: a line terminal, an MCP server, a test script today; a web page and REST later. Built on
[okay-simple](https://github.com/okay-effects/okay-simple)'s interface, for Scala 3 and 2.13, over any of its
implementations, with no dependency.

Status: `core` — `View`, `Action`, the effect `Face` (`offer`, `show`, `confirm`, `choose`, `fill`) and the step
machine, `steps` — `test`, the scripted face with back, and `term`, the terminal face — a line dialogue in okay-effects-io's `Console`, the person a handler of `Fields` asked field by field — are in; the MCP face is next, on okay-mcp. The data an
application speaks — `Json`, `Shape`, `Schema` — is okay-simple's (`json`, `shape`). Read
[specs/face-core.md](specs/face-core.md) and [specs/faces.md](specs/faces.md). The example is noughts and crosses in
`examples/` (`sbt "examples/runMain okay.face.examples.NoughtsTerminal"` to play); the first application is [verity](https://github.com/okay-effects/verity), a keeper of one's personal data.

```scala
import okay.effects.*
import okay.face.*
type F = Face
def greeting: String ! F =
  fill[Who, F](View.heading("Who are you?"), who).flatMap(w =>
    confirm[F](View.text(s"Greet ${w.name}?")).flatMap(yes => if yes then show[F](View.text(s"Hello, ${w.name}!")).map(_ => "greeted") else pure("left")))
// steps(greeting): a Step machine — Waiting(view, actions, resume) — that every face drives: the test face by a script,
// a terminal by prompts, an MCP server by tool calls; back is re-resuming an earlier Waiting
```
