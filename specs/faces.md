# The faces: test, terminal, MCP

Status: specification, 2026-10-20. Each face is a loop over `Step` (specs/face-core.md): show a view with its actions,
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
program is abandoned; `terminal` answers `None` in a variant `terminalOrQuit`). End of input is `q`.

## face-mcp — the Model Context Protocol over stdio

DONE 2026-10-20 as the `mcp` module, a BRIDGE onto okay-mcp (its own repository): `McpFace.server(name, version, steps)`
is an okay-mcp `Server` — the step's actions as tools with their JSON Schemas, `look`, `back`, the view a resource,
`list_changed` after each change; the dialogue's rest runs in the server's row, so `Link` is there for a consent by
elicitation. The single-tool fallback and the domain projection (`Describe`) are not done.

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

### The domain projection

Given `Describe[F]` and a handler for `F`, `tools/list` is `F`'s operations as tools, `tools/call` decodes `P`,
performs `make(p)` under the handler, encodes `X`. No session, no view. Both projections may be served by one process:
the dialogue's tools and the domain's, the names prefixed by the application (`verity_find`, `verity_get`).

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
