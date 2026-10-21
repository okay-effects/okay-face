# okay-faces — an application's capabilities as effects, its faces as handlers

An application has several faces: a screen for a person, tools for an agent, resources for a program. Each shows WHAT
IS THERE and WHAT CAN BE DONE. okay-faces writes that once, as a program over an algebraic effect, and each face is a
handler. Built on [okay-simple](https://github.com/okay-effects/okay-simple)'s interface, for Scala 3 and 2.13, over
any of its implementations; the data an application speaks — `Json`, `Shape`, `Schema` — is okay-simple's.

One program, these faces:

| face | module | what it is |
|---|---|---|
| test | `test` | a script of choices drives the step machine; what was shown, asserted |
| terminal | `term` | a line dialogue in okay-effects-io's `Console`; the person a handler of `Fields`, asked field by field; ANSI when asked |
| Swing | `swing` | a panel or a window on the JVM; forms from the shape; dressed by a `Look` |
| Laminar | `web/laminar` | Scala.js: the view drawn, forms from the shape, a click is `resume`, the browser's back; `Async` in the row to a `Future`; `web/airstream` and `web/waypoint` the bridges |
| remote | `web/laminar` | the same page over a dialogue on a server behind the REST face, the browser knowing no application |
| MCP | `mcp` | the dialogue as tools for an agent, on [okay-mcp](https://github.com/okay-effects/okay-mcp); a domain effect's operations as tools too |
| REST | `rest` | the dialogue as a JSON API with links and affordances; a domain effect as resources, and performed from afar over `Fetch` — on [okay-http](https://github.com/okay-effects/okay-http) |
| HTML | `html` | pages from the server, structural HTML5, no script; the look as `/okay.css` |

The look is data (`specs/style.md`): a `Theme` of tokens and a `Style` of rules by `Role`, compiled by each face; an
application's own words on views are typed marks (`Marks[M]` over a sealed set with a `Shape`), one source for the
look, the wire and the vocabulary an agent is told. The other role, a domain effect as a service: okay-simple's `Describe` projected here — `RestDomains`, `McpFace.domain`,
`remote`. Read [specs/face-core.md](specs/face-core.md), [specs/faces.md](specs/faces.md), [specs/web.md](specs/web.md).
The examples are noughts and crosses and a to-do list in `examples/` (`sbt "examples/runMain
okay.faces.examples.NoughtsTerminal"`, `NoughtsSwing` in a window, `TodoSwing`, `TodoHtml`, …); the first application is [verity](https://github.com/okay-effects/verity), a keeper of
one's personal data, on every face above; [okay-store](https://github.com/okay-effects/okay-store) describes its
types for them.

```scala
import okay.faces.*                      // all a program needs: the words are tagless, no implementation is named
import okay.effects.shape.Shape

case class Who(name: String)
val who = Shape.derived[Who].labelled("name", "Name")

val greeting: String ! Face =           // the one effect is the row
  fill[Who, Face](View.heading("Who are you?"), who).flatMap(w =>
    confirm[Face](View.text(s"Greet ${w.name}?")).flatMap(yes =>
      if yes then show[Face](View.text(s"Hello, ${w.name}!")).map(_ => "greeted") else pure("left")))

// every face is a handler of the one effect: a Step machine — Waiting(view, actions, resume) — driven by a script,
// by prompts, by tool calls, by requests. The implementation is said once, where the program is run:
object AtTheTerminal:
  import okay.effects.machine.Machine.self
  import okay.faces.term.Terminal
  def main(args: Array[String]): Unit = run(Terminal.console(greeting))
  // or SwingFace.handle(greeting), McpFace.stdio(greeting), HtmlFace.serve(greeting, port, host)…

// the test face plays it by a script, and tells what was shown:
import okay.effects.machine.Machine.self
import okay.faces.test.Plays
val plays = Plays()
plays.handle(greeting)(plays.Script.of(Chosen("submit", okay.effects.json.Json.obj("name" -> okay.effects.json.Json.Str("Ann"))),
    Chosen.plain("yes"))) match
  case plays.Played.Finished(views, a) => assert(a == "greeted" && views.last == View.text("Hello, Ann!"))
  case other => assert(false, other.toString)
```
