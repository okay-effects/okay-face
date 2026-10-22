# The faces: test, terminal, Swing, MCP, REST, HTML — and the web in specs/web.md

Status: built, 2026-10-20; the web faces in specs/web.md. Each face is a loop over `Step` (specs/face-core.md): show a view with its actions,
read a `Chosen`, resume. What differs per face is written here; everything else is face-core's.

## face-test — a face driven by a script

```scala
final case class Script(choices: List[Chosen])
sealed trait Played[A]
object Played {
  final case class Finished[A](views: List[View], a: A) extends Played[A]
  final case class Stuck[A](views: List[View], at: Step.Waiting[A, ?], rejected: Option[Rejected]) extends Played[A]  // script ended, or a choice was rejected
}
def play[A](step: Step[A, Pure], script: Script): Played[A]
def playOn[A, R[_]](step: Step[A, R], script: Script)(lower: Lowering[R]): Played[A]   // with a remaining row
```

`views` is every `View` shown, `Waiting`s and `Told`s alike, in order. A rejected choice stops the play and reports it:
a test that expects validation checks `Stuck(..., Some(rejected))`. Helpers: `Chosen.plain(name)`, `Chosen(name, obj(...))`,
`back` as a special script entry that re-resumes the previous `Waiting` (the face's history), so back is testable.

Law stated here: the same script on `face-test` and on `face-term` (with a scripted `Console`) shows the same `View`s.

## face-term — a line dialogue in the terminal

A program in okay-effects-io's `Console`, so it runs under `realConsole` and under `console(lines)` in tests:

```scala
def terminal[A, R[_]](step: Step[A, R]): A ! (R + Console)
```

Rendering `View` as text, deterministic and pipe-friendly, no colour unless a `Paint` is given (`Ansi(look)`, specs/style.md):

| View | Text |
|---|---|
| `Heading(1, s)` | `s` in capitals, underlined with `=`; level 2 with `-`; level 3 as `## s` |
| `Text` | wrapped at 78 columns |
| `Notice` | prefixed `info:`, `warning:`, `problem:` |
| `Fields` | `label: value`, labels padded to the longest |
| `Table` | columns padded to the widest cell, a header rule of `-` |
| `Items` | `- item` |
| `Group` | the title as a level-3 heading, children indented two spaces |
| `Ref` | `label [key]` |
| `Datum` | pretty JSON, indented |
| `Masked` | the hint, as it is |

Actions as a numbered menu after the view: `1) Play  2) Start over`, then the prompt `> `. The person types a number
or the action's name. Then, for an action with parameters, each field of its `Shape` is prompted in order:
`label (hint): `; a `Choice` is a sub-menu; `Bool` is `y/n`; `Optional` accepts an empty line as none; `Many` reads
lines until an empty one; a nested `Record` indents its fields. A field that does not decode is re-prompted with its
problem, so the person never re-types the whole form — validation is per field here, the whole `Chosen` is still what
`resume` sees. Secret fields are read without echo where the terminal allows it (`System.console().readPassword`),
plainly otherwise.

Commands at the menu prompt: `<` back (the face's history of `Waiting`s), `?` the actions' docs, `q` quit (the
program is abandoned; `terminal` answers `None` in a variant `terminalOrQuit`). End of input is `q`. An event offered
beside the actions (`Meanwhile`) is not listened to here: a line read blocks.

## face-mcp — the Model Context Protocol over stdio

DONE 2026-10-20 as the `mcp` module, a BRIDGE onto okay-mcp (its own repository): `McpFace.server(name, version, steps)`
is an okay-mcp `Server` — the step's actions as tools with their JSON Schemas, `look`, `back`, the view a resource,
`list_changed` after each change; the dialogue's rest runs in the server's row, so `Link` is there for a consent by
elicitation. The domain projection is `McpFace.domain` (specs/face-core.md, "Describe"); the single-tool fallback is
`McpFace.single`: for a client that reads the tool list once, `act(action, arguments)` is every step's action, `look`
and `back` as ever, the current actions with their schemas in every result's text and structured content —
`server(..., singleTool = true)`. The forms of every face take a list of records as items added and removed (Laminar,
Swing) or as slots, `min` and one spare, a blank one no item (HTML).

OVER HTTP too (2026-10-21, `McpFace.http.service(stream => server(...), path)`, on okay-http): the protocol's
streamable HTTP transport. `POST path` carries a message or a batch; the answer carries what the server says back —
as JSON when the client takes JSON, as a finite stream of events (`text/event-stream`, closed with the answer) when
it accepts one, and then the server's own messages ride in it: a `list_changed`, an elicitation. `initialize` opens a
session named by the `Mcp-Session-Id` header, each session its own server and dialogue; `DELETE path` ends it; `GET
path` is `405`, no open stream. THE LOOP IS THE STDIO ONE, `serve` over `Link`, handled by a request's mail — a
receive takes the next message that came, a send puts a line to go — and PARKED when the mail is empty: the handler
keeps the loop's continuation in the session, the response goes, the next POST resumes it. So an elicitation, a send
and then a receive, parks until the client's answer arrives: a question to the user crosses HTTP exchanges as it
crosses lines on stdio, by the continuation, no thread kept. `open` is told whether the client accepts a stream, and
only then should a server ask. It serves beside the REST and HTML faces on one port; verity's `api` mode has it at
`/mcp`, and asks the owner's consent by elicitation when the agent's client streams.

JSON-RPC 2.0, one message per line, over the process's stdin and stdout; stderr for logs. Our own `Json` and a hundred
lines of framing; no dependency. JVM and Native. Protocol version `2025-06-18`; `initialize` answers capabilities
`tools` (with `listChanged: true`), `resources`, and `elicitation` when the application asks for it.

### The dialogue projection

The server holds one session: the current `Step` and a history. `steps(program)` gives the first step at `initialize`.

- `tools/list` answers the current `Waiting`'s actions: each a tool `{name, description: doc or label, inputSchema:
  JsonSchema.of(params.erased)}`, plus two of the face's own: `look` (no parameters: the current view) and `back`
  (one step of history). After every change of step the server sends `notifications/tools/list_changed`.
- `tools/call` with an action's name is `Chosen(name, arguments)` → `resume`. The result's `content` is the next
  view, as `text` in Markdown (the terminal renderer's cousin: headings as `#`, tables as pipes, fields as a list) AND
  as a `structuredContent` object `{view: <View as JSON>, actions: [names]}`; `Told` views that passed on the way are
  prepended. A `Rejected` is an `isError: true` result listing the problems with their paths; the step is unchanged.
  `Done(a)` is a final result and the tool list becomes `look` alone.
- `resources/list`: one resource `face://view` (`text/markdown`) and `face://view.json` (`application/json`), the current
  view; `resources/read` returns them. `notifications/resources/updated` after each step.
- Fallback mode `--single-tool`, for clients that ignore `list_changed`: one tool `act` whose `inputSchema` is a
  `oneOf` over the current actions (`{action: name, args: {...}}`) regenerated per call, plus `look`; the `description`
  names the current actions.

The application's marks are told: `server(name, first, instructions, McpFace.Settings(vocabulary = marks.vocabulary))` appends "Marks on views, and what they
mean" to the instructions, lists `face://marks` among the resources (JSON, `[{name, doc}]`), and the Markdown of every
result shows a mark the vocabulary knows — `**price:** 12 €`, or `price: 12` in a cell — so the agent reads the
application's words, not only its parts. Without a vocabulary nothing changes.

### The domain projection

Built (`McpFace.domain(d)`): given `Describe[F]` and a handler for `F` in the row, `tools/list` is `F`'s operations as
tools `{domain}_{op}` with input and output schemas, `tools/call` decodes `P`, performs `make(p)` under the handler,
encodes `X`; a call that does not decode is an error result with the problems. No session, no view. `McpFace.both(a, b)`
serves the dialogue's tools and the domain's from one process (specs/face-core.md, "Describe").

### The person behind the agent

An interceptor over a domain effect may need the PERSON, not the agent: consent to reveal a secret. That is a
separate effect in the application (verity's `Consent`), not `Face`, because its audience is different. `face-mcp`
handles it by `elicitation/create` when the client declared the capability (the request carries a `Shape`'s JSON
Schema, the answer decodes through it), and by refusing with a clear error otherwise. `face-term` handles the same
effect at the console. So one program, under MCP, talks to the agent through `Face` and to the person through
`Consent`, and no line of the scenario knows which transport either took.

### Tests

A scripted stdio session in-process: lines in, lines out, golden. `initialize`; `tools/list` shows the game's actions;
`tools/call play {cell: 5}` answers the new board and `list_changed` follows; `tools/call play {cell: 99}` is an error
result and `tools/list` is unchanged; `back` restores the previous board; a finished game leaves `look` alone; the
single-tool mode plays the same script.

The `back` tool is listed only while there is a step to go back to (2026-10-21; it was listed whenever the dialogue
allowed back at all, and answered "nothing to go back to" nearly always once steps stopped being returnable by
default); `list_changed` follows every call, so the agent sees it come and go. The end of a dialogue is shown as
"done" (`Session.over`, an info notice) by every face that serves sessions, by Swing and by Laminar: the program's
value is the program's, not shown — a program shows what it wants seen before it ends.

### Words, sessions — 2026-10-21

THE FACES' OWN WORDS to a person — Back, Help, done, Add, Remove, the terminal's hints and prompts, the lost page's
lines — are `Words`, carried by the `Look` (`Look(theme, style, words)`), English by default: an application gives its
own in its look, and every face that takes a look speaks them; the terminal takes `words` beside its paint. What a
program says is the program's. MCP's and REST's messages to a client stay English: they are for a program.

SESSIONS of the faces that serve many counterparts (HTML, REST, MCP over HTTP) are kept by `Sessions` in core: random
ids (128 bits as hex, not guessed from one another), each forgotten when idle an hour or when more than a thousand are
held — a server that runs for long keeps no visitor forever. A test names them: `ids = Sessions.counting("s")`, on
`pages`, `sessions` and `handle`.

THE TERMINAL asks an optional leaf at once, "empty for none" in its hint, instead of "due? (y/n)" and then the value;
an optional record or list is still asked for first.

## face-swing — a panel, a window

DONE 2026-10-20 as `swing` (the JVM): `SwingFace(faces).panel(steps, done, look)` is a `JPanel` rebuilt at every step —
the view as components named by their `Role`s, a plain action a `JButton` named `action:name`, an action with
parameters a form from its erased schema (`submit:name`), back re-resumes the previous `Waiting`; `window(title, ...)`
puts it in a frame on the event dispatch thread, the title following `Navigation` when `pages` are given. With a `Look`
the components are dressed by `Looks` after each rebuild; without, the platform's look and feel. Tested headless.
The handler (`SwingFace.handle(p)`) makes what the program tells before its first offer (`Step.opening`) the window's
HELP: a `?` button at the top right opens it in a dialog (2026-10-21, after the rules above the first board scrolled
away at the first click); what is told later in the dialogue stays inline, as part of the step it precedes.

## face-rest — the dialogue as a JSON API

DONE 2026-10-20 as `rest`, on okay-http: `RestFace(faces).sessions(open, base)` — `POST base` opens a session and
answers its resource, `GET base/{id}` the view as JSON with the actions as links (JSON Schema for any client) and as
affordances (the exact `Schema`, for the remote face), `POST base/{id}/act/{name}` with the arguments is `resume`
(a 400 problem leaves the step), `POST base/{id}/back`. `RestDomains` is the domain projection: a described effect
as resources, and performed from afar over `Fetch`.

## face-html — pages from the server

DONE 2026-10-20 as `html`, on okay-http: `HtmlFace(faces).pages(open, title, base, look)` — `GET /` opens a session
and redirects, `GET base/{id}` the page (structural HTML5 that is well-formed XML, a class per `Role`, no style
inline, no script), a plain action a button, an action with parameters a form from its erased schema, back a button,
`POST base/{id}/act/{name}` is `resume` then a redirect (post, redirect, get), the problems shown on the same page;
the look at `GET /okay.css`, compiled from the `Look`. The HTML suite checks every page for well-formedness. What was
told since the last step comes from the dialogue apart from the step's view (`Dialogue.shown`, 2026-10-21) and is set
in `told` sections above it; the step's own view is rendered as it is, its refs live wherever they are in it.
