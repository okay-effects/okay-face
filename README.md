# okay-faces — an application's capabilities as effects, its faces as handlers

An application has several faces: a screen for a person, tools for an agent, resources for a program. Each shows WHAT
IS THERE and WHAT CAN BE DONE. okay-faces writes that once, as a program over an algebraic effect, and each face is a
handler: a line terminal, an MCP server, a test script today; a web page and REST later. Built on
[okay-simple](https://github.com/okay-effects/okay-simple)'s interface, for Scala 3 and 2.13, over any of its
implementations, with no dependency.

Status: `core` — `View`, `Action`, the effect `Face` (`offer`, `show`, `confirm`, `choose`, `fill`) and the step
machine, `steps` — `test`, the scripted face with back, and `term`, the terminal face — a line dialogue in okay-effects-io's `Console`, the person a handler of `Fields` asked field by field — `web/laminar`, the Laminar face (Scala.js: the view drawn, actions buttons and forms from the shape, a click is `resume`, back, the browser's back; `Async` in the row to a Future), with the bridges `web/airstream` and `web/waypoint`, and `swing`, the Swing face (the JVM: a panel, a window), `mcp`, the MCP face (the dialogue as tools for an agent, on [okay-mcp](https://github.com/okay-effects/okay-mcp)), and `rest` and `html`, the dialogue as a JSON API with links and as pages from the server, on [okay-http](https://github.com/okay-effects/okay-http), are in: one program, seven faces. The look is data too (`specs/style.md`): a `Theme` of tokens and a `Style` of rules by `Role`, compiled by each face — a stylesheet for the web (`GET /okay.css`), fonts and colours in Swing, ANSI in the terminal when asked. And `web/laminar`'s remote face: the same page over a dialogue on a server behind the REST face, the browser knowing no application — write it once, pick Laminar and REST. The other role, a domain effect as a service: okay-simple's `Describe` (an `Op` per operation, shapes both ways) projected here — REST resources (`RestDomains`), MCP tools (`McpFace.domain`), and performed from afar over `Fetch` (`specs/face-core.md`, "Describe"). [verity](https://github.com/okay-effects/verity) runs on them. The data an
application speaks — `Json`, `Shape`, `Schema` — is okay-simple's (`json`, `shape`). Read
[specs/face-core.md](specs/face-core.md) and [specs/faces.md](specs/faces.md). The example is noughts and crosses in
`examples/` (`sbt "examples/runMain okay.face.examples.NoughtsTerminal"` to play at the terminal, `NoughtsSwing` in a window); the first application is [verity](https://github.com/okay-effects/verity), a keeper of one's personal data.

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
