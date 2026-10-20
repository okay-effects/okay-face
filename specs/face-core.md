# face-core — an application's capabilities as effects, its faces as handlers

Status: specification, 2026-10-20. No code yet. Decided with the operator in conversation; the decisions are marked
DECIDED, what is still open is under "Open".

## Overview

An application has several faces: a screen for a person, a list of tools for an agent, resources for a program. Each
answers one question: WHAT IS THERE NOW, AND WHAT CAN BE DONE. A screen shows data and buttons, some disabled; a REST
answer in the HATEOAS style carries a representation and links to the actions available; an MCP server lists
resources and tools; GraphQL a schema of queries and mutations; a Delphi form, components with `Enabled` on their
actions. So the axis of this library is not "a screen" but A REPRESENTATION WITH ITS AFFORDANCES, and the screen, the
REST answer and the tool list are three projections of it.

Algebraic effects are the mechanism, and nothing is added to them. An effect is a sealed trait of operations, each
operation data with its answer type: that is a PORT, in the hexagonal sense, and a handler is an ADAPTER. One effect
may be handled by a native implementation, exposed outward over a transport, consumed from outward over a transport,
or shown to a person — four handlers (or performers) of one data type. "What is done" is the effect; "how" is the
handler. Translating between levels of abstraction is an ordinary handler whose clauses perform the operations of the
level below.

Two roles meet here, and a design that keeps only one is wrong:

- In a DIALOGUE the application leads and the counterpart answers: the program performs `Offer(view, actions)` and is
  answered with the action taken. That is a console program's `println`/`readLine`, Delphi's `ShowModal`, an MCP
  server's elicitation. A long-lived screen is a recursive loop of offers — Elm's architecture, as a style of
  writing a program rather than a framework.
- In a SERVICE the counterpart leads: an HTTP request or a tool call PERFORMS a domain operation on the caller's
  behalf, and the application handles it. The same domain effect, the other way round.

The upward direction a UI seems to need (a click becoming a high-level command) needs no mechanism of its own: an
operation is a request with a continuation, and the answer IS the upward path. The handler stores `k`, draws, and
resumes `k` when the person or the agent acts.

## Only okay-simple underneath

DECIDED: okay-faces depends on okay-simple alone — `okay-effects` and its modules, released — and on nothing else: no
JSON library, no web library in the core, and NOT on okay, the platform. The idea is okay-simple's own: one simple,
released floor that works with anything, and the next floor built the same way. okay (Scala 3, on okay-core, not yet
released, large) has floors that look alike — `okay.ui` with a view value and hosts, `okay.codec.Schema` with a form
from a schema, `okay-mcp` — and okay-faces must be complete WITHOUT them: its own typed schema, its own semantic view,
its own faces for the terminal, MCP, tests and, later, the web. Working with okay is possible and welcome, as
okay-simple works with cats or ZIO: an `interop/` module per bridge (`View` and actions lowered into `okay.ui.Ui`,
`okay.codec.Schema` into `Shape`, our tools as `okay-mcp`'s `ToolSpec`), optional, Scala 3, never required by anything
here. What okay already learned is borrowed as DESIGN, not as a dependency: a view is a value with keys and no
functions; a form is rendered from the same schema that decodes it; an MCP server is a handler.

## The four data types

Everything is built from four data types, none knowing any platform. MOVED 2026-10-20: `Value` (now `Json`), `Shape`,
`Schema` and `JsonSchema` are okay-simple's modules `json` and `shape` (docs/modules/json.md, shape.md there), because
every consumer — okay-faces' forms, okay-mcp's tool schemas, verity's kinds — needs them and none should depend on
another for them. Their description below is kept as the design record; the code is okay-simple's, as are the two
next steps, reading a shape as an effect and derivation from a type (okay-simple's BACKLOG.md, "shape:"). okay-faces
keeps `View` and `Action`, the effect and the step machine. All are VALUES: no functions inside `Value`,
`View` and the erased `Schema`, so they have equality and cross a wire. `Shape[P]` and `Action[A]` carry functions
(decoders and constructors) and stay in the program; their erasures are values.

### Value — the neutral datum

```scala
sealed trait Value
object Value {
  case object Null extends Value
  final case class Bool(b: Boolean) extends Value
  final case class Num(n: BigDecimal) extends Value
  final case class Str(s: String) extends Value
  final case class Arr(items: Vector[Value]) extends Value
  final case class Obj(fields: Vector[(String, Value)]) extends Value   // ORDERED: the order a form shows, a wire writes
}
```

JSON-shaped on purpose: every transport and every face speaks `Value`; a typed `P` exists only inside the program,
after decoding. `Json.print(v): String` and `Json.parse(s): Either[Problem, Value]` are in face-core (a hundred and
fifty lines, no dependency) because `face-mcp`, files and tests all need them.

### Problems — what a decoder says

```scala
final case class Problem(path: List[String], message: String)        // path: field names, indices as strings
type Problems = List[Problem]                                         // never empty when returned
```

### Shape[P] — a typed schema

One `Shape` is at once a FORM for a person (fields, labels, constraints), an `inputSchema` for an MCP tool, an input
type for GraphQL, a request body for REST, flags for a CLI, and the DECODER that validates what any of them sends.
This is the unification point of the whole library: write the shape once, every face derives from it.

```scala
sealed trait Shape[P] {
  def decode(v: Value): Either[Problems, P]                           // EVERY problem, with its path, not the first
  def encode(p: P): Value
  def erased: Schema                                                  // the value, for files and wires
  def absent: Option[P]                                               // what an absent field is: `Optional` says `None`, the rest nothing
  def map[B](to: P => B)(from: B => P): Shape[B]; def refine[B](to: P => Either[String, B])(from: B => P): Shape[B]
  def check(message: String)(ok: P => Boolean): Shape[P]; def described(label: String, doc: Option[String] = None): Shape[P]
  def optional: Shape[Option[P]]; def many(min: Int = 0, max: Option[Int] = None): Shape[List[P]]
}
object Shape {
  // primitives, each with constraints as DATA (so a face can show them before decoding)
  final case class Text(constraints: List[Constraint.OnText] = Nil) extends Shape[String]
  final case class Integer(constraints: List[Constraint.OnNumber] = Nil) extends Shape[BigInt]
  final case class Number(constraints: List[Constraint.OnNumber] = Nil) extends Shape[BigDecimal]
  case object Bool extends Shape[Boolean]
  case object Date extends Shape[Day]                                  // `Day(y, m, d)`, ISO text on the wire — our own, no java.time (Scala.js, Native)
  final case class Choice[A](options: List[(String, A)]) extends Shape[A]  // one of a closed list, by name
  final case class Optional[A](of: Shape[A]) extends Shape[Option[A]]  // `Null` or absent is `None`
  final case class Many[A](of: Shape[A], min: Int = 0, max: Option[Int] = None) extends Shape[List[A]]
  final case class Record[P](fields: Fields[P], write: P => Vector[(String, Value)]) extends Shape[P]   // an `Obj`
  /** a tagged union: `{"type": name, ...the case's fields}` for a record case, `{"type": name, "value": v}` otherwise */
  final case class Sum[A](cases: List[Case[A]]) extends Shape[A]
  /** a refinement or a conversion: `to` may fail with a message (one `Problem` at this path) */
  final case class Mapped[A, B](of: Shape[A], to: A => Either[String, B], from: B => A) extends Shape[B]
  /** the same shape with words for a person: label, doc; a face shows them, a decoder ignores them */
  final case class Described[A](of: Shape[A], label: String, doc: Option[String] = None) extends Shape[A]
  /** a loose shape: any `Value`, validated against an erased `Schema` (user-defined kinds in a file) */
  final case class Loose(schema: Schema, defs: Map[String, Schema] = Map.empty) extends Shape[Value]
  case object AnyValue extends Shape[Value]
  /** a shape named and given later: how a recursive shape is written; erased to `Schema.Ref(name)` */
  final case class Deferred[A](name: String, of: () => Shape[A]) extends Shape[A]

  /** a record's fields: a free applicative, so one value `P` is built from fields in order, every field's problems kept */
  sealed trait Fields[P]
  object Fields {
    final case class Pure[P](p: P) extends Fields[P]
    final case class Ap[A, P](f: Fields[A => P], field: Field[A]) extends Fields[P]
  }
  final case class Field[A](name: String, shape: Shape[A], label: Option[String] = None, doc: Option[String] = None,
                            secret: Boolean = false, default: Option[A] = None)   // `.labelled`, `.documented`, `.asSecret`, `.withDefault`
  /** a case of a `Sum`: its name, the shape of its value `B`, `inject: B => A`, `project: A => Option[B]` */
  sealed abstract class Case[A] { type B; ... }
}
```

Words: `text`, `integer`, `int`, `long`, `number`, `bool`, `date`, `any`, `choice(...)`, `field(name, shape)`, `unit`,
`sum(cases*)`, `sub[A, B <: A](name, shape)(project)` (a case whose value is a subtype), `Case(name, shape)(inject)(project)`,
`deferred(name)(shape)`, `loose(schema, defs)`, and the records, generated for arities 1 to 12 (`sbt records`, project/Records.scala):

```scala
record(field("name", text), field("age", int))(Person(_, _))(p => (p.name, p.age)): Shape[Person]
```

`make` builds the value, `unmake` takes it apart for `encode` — the free applicative can decode but not encode on its
own, so a record carries both. Cross-field validation is `record(...).check(message)(ok)`. A missing field decodes to
its `default`, else to its shape's `absent` (`None` for `Optional`), else it is `required`; fields the shape does not
name are ignored.

Interpreters over `Shape` (the form, the JSON Schema, the terminal prompts) TRAVERSE the nodes; `Case`'s `B` is an
abstract type member, not an existential, so both compilers match it. Decoding is each node's own method, so the
decoder needs no GADT matching at all.

#### Reading a shape is an effect — DECIDED 2026-10-20, for step 2

`decode(v: Value)` fixes the source of the fields to a JSON-shaped value, and that is a leak: nothing in a `Shape` says
JSON. Reading the fields a shape names is a PROGRAM over an effect — "the field `name`, as text" — and where the field
comes from is the handler's business: a `Json` (okay-simple's `okay.effects.json.Json`, which replaces `Value` here
once okay-simple 0.1.1 is released), CBOR or any other encoding later, a command line's arguments, or a PERSON asked
field by field at a terminal. That last handler is the point: the terminal face's loop of prompt, input, problem and
prompt again is a handler of the field-reading effect, not code of its own, and the same handler serves every form.

```scala
sealed trait Fields[X]                                   // the effect: a primitive at a path, as the shape wants it
object Fields {
  final case class Read[X](path: List[String], prim: Prim, answer: Option[Either[Problem, Any]] => X) extends Fields[X]  // typed per Prim in code
  final case class Enter[X](path: List[String], answer: Unit => X) extends Fields[X]   // into a record or a list element
  final case class Leave[X](answer: Unit => X) extends Fields[X]
  final case class Size[X](path: List[String], answer: Option[Int] => X) extends Fields[X]  // of a list
}
def read[P, R[_]](shape: Shape[P])(implicit in: In[Fields, R]): Either[Problems, P] ! R   // every problem kept, as now
def fromJson(j: Json): handler of Fields answering from the value           // today's `decode`
def write[P](shape: Shape[P], p: P): Json                                    // encoding stays a function: the shape knows its wire
```

`Shape` itself stays DATA, because the erased `Schema`, the JSON Schema and the form are derived from it; what becomes
an effect is the reading. Writing stays a function to a value for now (a `Write` effect, for a face that shows a value
field by field, when one needs it). `decode(v)` remains as `run(fromJson(v)(read(shape)))` for convenience.

#### Schema — a Shape erased to a value

```scala
sealed trait Schema   // Text(constraints), Integer, Number, Bool, Date, Choice(names), Optional(of), Many(of, min, max),
                      // Record(fields: Vector[Schema.Field]), Sum(cases), Described(of, label, doc), Ref(name), Any
```

`shape.erased: Schema` drops decoders and constructors; `Schema.loose(s, defs): Shape[Value]` validates a `Value`
against a `Schema` with no typed result, NORMALISED as the typed shape would write it back (fields in the schema's
order, known fields only, an absent optional as `Null`, a date in ISO) — so typed decode-then-encode and loose decode
agree, which a test states. This duality is what lets an application store a user-defined kind of record in a file as
data and still derive its form, its tool schema and its validation. `Schema.shape: Shape[Schema]` is the meta-schema,
recursive through `Deferred("schema")`, so a file of schemas is read with the same decoder as everything else, and
`Schema.defs` gives its one definition for `JsonSchema.of`.

#### JSON Schema

`JsonSchema.of(s: Schema, defs: Map[String, Schema] = Map.empty): Value` — a JSON Schema draft 2020-12 object:
`type`, `properties`, `required`, `enum`, `minimum`, `maxLength`, `format: date`, `title` and `description` from
`Described` and a field's words, `default`, `items`, `oneOf` with a `type` tag for a `Sum`, `$ref` into `$defs` for a
`Ref`. What MCP, OpenAPI and most form libraries read. An optional field is its inner schema and not required. Secret
fields are marked `"x-secret": true`, nothing more: the policy is the application's.

### View — the semantic representation

```scala
sealed trait View
object View {
  case object Empty extends View
  final case class Text(s: String) extends View                              // a paragraph
  final case class Heading(level: Int, s: String) extends View               // 1..3
  final case class Notice(level: Notice.Level, s: String) extends View       // Info, Warning, Problem
  final case class Fields(rows: Vector[(String, View)]) extends View         // label → value, a record shown
  final case class Table(columns: Vector[String], rows: Vector[Vector[View]]) extends View
  final case class Items(items: Vector[View]) extends View                   // a list
  final case class Group(title: Option[String], children: Vector[View]) extends View
  final case class Ref(key: String, label: String) extends View              // names an entity; a face may link it
  final case class Datum(v: Value, schema: Option[Schema]) extends View      // a structured value shown as it is
  final case class Masked(hint: String) extends View                         // a secret NOT shown ("•••• 4821")
}
```

DECIDED: semantic only, no layout, no colour, no widths. Like semantic HTML or Markdown, not like CSS. Layout hints,
when Laminar or okay-ui need them, are a separate layer added then. A `View` never contains a function and never
contains a secret the program did not decide to show; `Masked` is how a secret is present without being shown.

### Action[A] — one affordance

```scala
sealed abstract class Action[A] {
  type P
  def name: String                    // the wire name: `[a-z][a-z0-9_-]*`, unique within one Offer
  def label: String                   // for a person
  def doc: Option[String]             // for a person and an agent
  def params: Shape[P]
  def make: P => A
}
object Action {
  def apply[P, A](name: String, label: String, params: Shape[P], doc: Option[String] = None)(make: P => A): Action[A]
  def plain[A](name: String, label: String, doc: Option[String] = None)(a: => A): Action[A]   // no parameters
}
```

A list of actions is the affordances of the current state: Delphi's enabled buttons, HATEOAS links, an MCP tool list.
An action's `Shape` is its form, its `inputSchema`, its flags.

## The effect

```scala
sealed trait Face[X]
object Face {
  /** show a representation and the actions available; answered by the action taken, its parameters decoded */
  final case class Offer[A, X](view: View, actions: List[Action[A]], answer: A => X) extends Face[X]
  /** show, and go on; nothing to wait for (`Show`, not `Tell`: the standard Writer's word is `tell`) */
  final case class Show[X](view: View, answer: Unit => X) extends Face[X]
}
```

Scala 2 has no GADT inference for a method's type parameter, so each operation carries `answer`, as every effect in
okay-simple does. Words, in a class over any `Effects` (the module pattern: `class Faces[F <: Effects](implicit fx: F)`,
`okay.face` the package object on the machine):

```scala
def offer[A, R[_]](view: View, actions: List[Action[A]])(implicit in: In[Face, R]): A ! R
def show[R[_]](view: View)(implicit in: In[Face, R]): Unit ! R
// built on them
def confirm[R[_]](question: View): Boolean ! R                        // yes / no
def choose[A, R[_]](view: View, options: List[(String, A)]): A ! R    // one plain action each
def fill[P, R[_]](view: View, form: Shape[P]): P ! R                  // one action "submit" (`fill`, not `ask`: the Reader's word)
def fillOrCancel[P, R[_]](view: View, form: Shape[P]): Option[P] ! R  // "submit" and "cancel"
```

A scenario is a program in `Face + Domain`, the domain effects the application's own. A COMPONENT is a function
returning a program in `Face` with a result — `ShowModal`. A component library is functions and shared `View`s and
`Shape`s. Nothing here knows a button, HTTP or an agent.

### Chosen and Rejected

```scala
final case class Chosen(action: String, args: Value)                 // what a face sends back: a name and an `Obj`
final case class Rejected(problems: Problems)                        // unknown action, or the args did not decode
```

## The step machine — one core for every face

The standard handler of `Face` draws nothing. It turns a program into a machine of steps:

```scala
sealed trait Step[A, R[_]]
object Step {
  final case class Done[A, R[_]](a: A) extends Step[A, R]
  /** shown, and waiting: `resume` decodes the choice against `actions` — `Left(Rejected)` and the face shows the
   * problems and waits on THIS step again — or goes on, a program of the remaining row */
  final case class Waiting[A, R[_]](view: View, actions: List[Action[?]], resume: Chosen => Either[Rejected, Step[A, R] ! R]) extends Step[A, R]
  /** told, and going on at once; a face shows it and continues (MCP gathers notices into the next answer) */
  final case class Told[A, R[_]](view: View, next: Step[A, R] ! R) extends Step[A, R]
}
def steps[A, R[_]](p: A ! (R + Face)): Step[A, R] ! R           // `handleLast` with the stepping handler
```

The handler is a general `Handler[Face, R, A, Step[A, R]]`: `ret(a)` is `Done(a)`; `op(Offer(v, as, answer), k)` is
`Waiting(v, as, chosen => decode(chosen, as).map(a => k(answer(a))))`; `op(Tell(v, answer), k)` is
`Told(v, k(answer(())))`. `k` is deep and resumable any number of times: a face that keeps earlier `Waiting`s has
BACK for free — resuming an older `resume` runs the program again from there. Whatever stateful handlers were applied
INSIDE (below `Face`) have their state inside `k`, so back restores it; the world's handlers outside do not roll
back. That is the honest statement of what back can and cannot do, and it is the application's choice where a handler
goes.

Every face is now a loop over `Step`, and the face-specific code is two things: SHOW `view` with `actions`, and READ a
`Chosen`. The terminal prints and blocks. Laminar puts `resume` into a click handler and returns to the event loop.
The MCP server serves `view` as a resource and `actions` as tools, and a tool call is `resume`. REST serves the
representation with links, and a request carrying a continuation id is `resume`. The same scenario program is
literally one, which `face-test` checks by driving the machine with a script.

The remaining row `R` is what the face must still run: DECIDED, the recommended structure is to handle every STATEFUL
effect inside, before `Face`, and leave in `R` only effects answered in place by stateless handlers (okay-effects-io's
`Console`, `Clock`, `Lift[Future]`), which a face applies to each step's program alike. A face is given a `Runner` and
how to run `X ! R`: `Id` on the terminal, `Future` for Scala.js and MCP (`Runner.lower`).

### Validation lives in the face, not in the scenario

A person types a bad value; an agent calls a tool with bad arguments. The scenario must not know. `resume` decodes
through the action's `Shape`; a `Rejected` makes the face show the problems and wait on the same step: per field in
the terminal (re-prompt that field), as field errors on a web form, as an error result to the agent. The program
receives a typed `P`. The validation loop that every UI framework reinvents is the decoder, once.

## Describe — a domain effect as a service (the second role)

For the service role an effect needs a description as data:

```scala
trait Describe[F[_]] {
  def name: String
  def operations: List[Describe.Op[F]]
}
object Describe {
  sealed abstract class Op[F[_]] {
    type P; type X
    def name: String; def doc: Option[String]
    def params: Shape[P]; def result: Shape[X]
    def make: P => F[X]                   // the operation, with the identity answer — as `perform` makes it
  }
}
```

With `Describe[Orders]` an MCP server serves `Orders`' operations as tools, REST as a resource, GraphQL as queries
and mutations, a CLI as subcommands; the call decodes `P`, performs `make(p)`, encodes `X`. Written by hand first;
derivation from the GADT by Scala 3's `Mirror` later, in a Scala 3 module (cross-building stops it in the core).
Authorisation and audit are interceptors (`observe`, `around`) over the domain effect and do not care which face
performed it. `Describe` is step 5 of the plan; its interface is fixed here so `face-mcp` is built with it in mind.

## Navigation — routes as an effect, of no platform (2026-10-20)

`Navigation[P]`: `Push(page)`, `Replace(page)`, `Current` — a page any value the application names its places by. The
program knows no router, no window, no URL; the handler is the face's: Waypoint's `Router` in a browser
(`web/waypoint`'s `router`, the URL and the browser's history), `memory(pages)` anywhere — a stack in memory, `Pages`,
that tells the face of each change, so a Swing window's title or a terminal's line follows the page, and `back()` for
the face's own back — and `recording(first)` for a test. The law: the same program under `recording`, `memory` and
`router` asks for the same pages.

## Laws

1. `View`, `Value`, `Schema`, `Chosen` are values: no functions, structural equality, printable as JSON.
2. A `View` carries no secret the program did not put there; `Masked` is the presence of a secret.
3. A step's `actions` are the ONLY things a face may send back; a `Chosen` naming anything else is `Rejected`.
4. Decoding is the face's; a scenario sees typed values only.
5. Back is the face's: a history of `Waiting`s. A scenario may also offer `cancel` actions; the two are different.
6. Two faces given the same script of `Chosen`s produce the same sequence of `View`s (the test face states it).
7. Nothing in okay-faces but `interop/` imports okay, the platform; nothing in face-core imports a platform or a JSON
   library. Every face works with okay-simple alone.

## Scala 2 and 3

One source for both, in the shared style of okay-simple: operations with `answer`; rows written as
`({ type L[x] = Sum[R, Face, x] })#L` in library source, `R + Face` in Scala 3 user code and `(R + Face)#L` in Scala 2;
abstract type members (`Action#P`) rather than existential types; `Fields` arities generated. The `scala-3/` folder may
hold conveniences (context bounds, `Mirror` derivation goes to a separate Scala-3-only module).

## Modules

| Module | Artifact | Package | Content | Depends on |
|---|---|---|---|---|
| `core/` | `okay-faces` | `okay.face` | View, Action, Face, Step, Describe; Role, Theme, Style, Look, Css (specs/style.md); Affordance, ViewJson (the wire) | `okay-effects`, `okay-effects-shape` (and `json` through it) |
| `test/` | `okay-faces-test` | `okay.face.test` | the scripted face: `Script`, `play`, assertions on views | core |
| `term/` | `okay-faces-term` | `okay.face.term` | the line terminal face, a program in `Console` | core, `okay-effects-io` |
| `mcp/` | `okay-faces-mcp` | `okay.face.mcp` | JSON-RPC over stdio; dialogue and domain projections | core (JVM, Native) |
| `examples/` | — | `okay.face.examples` | noughts and crosses; the greeting form of the docs | core, test, term, mcp |

Later, all here and on okay-simple alone: `web/` — `airstream`, `laminar`, `waypoint` (moved from okay-simple's
backlog, specs/web.md) and `face-laminar`, the Laminar face over them; `screen/` (a keyed widget tree between `View`
and the web platforms, with the diff) if the Laminar face needs it; `rest/`, `graphql/`. And `interop/okay-ui`, `interop/okay-codec`, `interop/okay-mcp`: optional bridges to okay,
Scala 3 only, as okay-simple's interop modules are to cats and ZIO.

Build: sbt, cross `3.3.8` / `2.13.18`, `crossproject` for JVM always, JS and Native when asked, as okay-simple's build;
`okay-effects` 0.1.0 from the okay-effects Pages repository; munit; zero other dependencies. `organization
io.github.okay-effects`, Apache-2.0.

## The example: noughts and crosses

Tiny on purpose, and it exercises every piece: a loop of offers, an action with a parameter (`play(cell)`, a `Choice`
of the free cells), branching (win, draw, go on), `Tell` at the end, back as undo through the face's history, and an
agent playing it over MCP as the first demonstration that the same program has two faces.

```scala
def game[R[_]: Has[Face]](board: Board): Outcome ! R =
  board.outcome match
    case Some(o) => tell[R](show(board, o)).map(_ => o)
    case None =>
      offer[Move, R](show(board), List(
        Action("play", "Play", choice(board.free.map(c => c.toString -> c)))(Play(_)),
        Action.plain("restart", "Start over")(Restart)))
        .flatMap { case Play(c) => game(board.played(c).replied); case Restart => game(Board.empty) }
```

The docs' two-field greeting form (`name`, `language`) shows `Shape` and `ask`.

## Tests

- `Shape`: decode/encode round trips for every node; problems carry paths; `Loose` agrees with the typed decoder on
  the same `Schema`; `JsonSchema.of` matches golden files; `Json` parses what it prints and rejects what it must.
- `Step`: a three-screen scenario with a branch gives the expected `View`s for a given script; a `Rejected` leaves the
  machine on the same step; resuming an older `Waiting` replays (back); a `State` handled inside is restored by back.
- Both compilers, on the machine and on `Free` (the examples run on both, as okay-simple's do).
- `face-term`: the example played through a scripted `Console` end to end, the transcript golden.
- `face-mcp`: a scripted stdio session: `initialize`, `tools/list` shows the first step's actions, a `tools/call` moves
  the game, `list_changed` is sent, a bad argument is an error result and the tool list is unchanged.

## Order of work

1. Value, Json, Problem, Shape, Schema, JsonSchema. Verify: the Shape tests above, both compilers. DONE 2026-10-20,
   then MOVED to okay-simple (`json`, `shape`) the same day; okay-faces depends on them.
2. View, Action, Face, Step, `steps`; `face-test`. Verify: the Step tests above. DONE 2026-10-20: `Step[A, M[_]]` is
   parameterised by the program type of the remaining row (`[X] => X ! Rest`), `resume` gives the rest as a program and
   the face runs it; `Faces[F]` over any implementation, `Plays[F]` the test face over a `Runner` with `Pick`/`Back`
   entries; laws: a state handled inside Face comes back with Back, the machine and Free show the same views.
3. `face-term`; the example on it. Verify: played interactively, and from a file on stdin. DONE 2026-10-20: `Terminal[F]`
   over any implementation, a program in `Console`; `prompting` the person as a `Fields` handler (presence asked only for
   optionals; a choice's names before menu numbers); tests under a scripted console on the machine and Free.
4. `face-mcp`, dialogue projection. Verify: Claude Code or the MCP inspector plays the example.
5. `Describe`, domain projection in `face-mcp`. Verify: a described effect appears as tools with no scenario.
6. verity (its own repository) as the first real consumer; then screen and the web faces; the okay bridges when
   someone needs them.

After step 4 the model holds or it does not, before any web platform is paid for.

## Open

- External events (a timer, a server push) while a step waits: as an alternative in `actions` with no actor, or a
  second effect merged by the concurrent module. Deferred until verity's sync needs it.
- Non-local jumps (a global menu from any screen): `Abort`/`Error` unwinding, or a history jump by the face. The
  example application decides.
- Whether `Schema` should carry layout hints at all, or they belong to a `screen/` layer only.
