# Style: how it looks, said once

A `View` says WHAT is shown and a `Role` says WHAT A PART IS; neither says how it looks. The look is data of its own,
in `core`, and every face interprets it: the web as a stylesheet, Swing as fonts and colours on components, the
terminal as ANSI when asked. One look, every face — the thesis of the library applied to its appearance.

## The three values

```scala
final case class Theme(palette: Palette, font: Family, size: Int, space: Int, measure: Int)  // the tokens
final case class Style(rules: Map[Role, Rule])                                                 // a rule per role
final case class Look(theme: Theme, style: Style)                                              // what a face is given
```

- **Theme**: the tokens. A `Palette` of eight `Tone`s (`Fg`, `Bg`, `Muted`, `Surface`, `Border`, `Accent`,
  `Warning`, `Problem`), a `Family` (`System`, `Serif`, `Mono`), the base `size` (px, points), the unit of `space`
  (px), and the `measure` (characters of text across). `Theme.light`, `Theme.dark`.
- **Style**: a `Rule` per `Role`. A rule names TONES, never colours — `color`, `background`, `border` are `Option[Tone]` —
  with `bold`, `size` (times the base), `mono`, `pad` and `gap` (units of space), `flow` (`Column`, `Row`) and
  `centered` (the content in the middle of the part: `justify-content`/`text-align` on the web, the alignment in Swing). So one
  style stands under any theme, and a dark theme is a change of palette, not of rules. `Style.default` is what a
  dialogue needs to be read; `Style.plain` says nothing; `adjust(roles*)(rule => …)` changes the rules named, the same way.
- **Look**: the two together. `Look.default`, `Look.dark`.

What is NOT here: layout. Where things go — a column, a grid, a window — is each face's own; the look says what the
parts are made of. A style that could place things would be a second CSS, and the library would be writing it for
every face. DECIDED: tokens and roles, nothing more.

## Roles

`Role` is the vocabulary a face marks its output with: a class on the web (`okay-heading okay-heading-1`), a name in
Swing (`okay-heading-1`), a paint in the terminal. `names` run general to specific, so a stylesheet may address all
headings or one level. The roles: `Face`, `View`, `Told`, `Text`, `Heading(1..3)`, `Notice(level)`, `Fields`,
`Label`, `Table`, `Items`, `Group`, `GroupTitle`, `Ref`, `Datum`, `Masked`, `Problems`, `Actions`, `Action`, `Form`,
`Doc`, `Field`, `Back`. `Role.all` lists them; `Role.byClass` finds one back by its class.

The HTML and Laminar faces mark the SAME classes, so one stylesheet serves both; Swing names its components by the
same words, and the Swing tests find them by those names. The vocabulary is a contract, as `ViewJson` is for REST:
an application may write its own stylesheet against it instead of taking the compiled one.

## Marks: the application's own words

The roles are the library's; an application has words of its own — "price", "overdue", "badge" — and says them as
MARKS: `View.Marked(mark, view)` is a view with the application's word on it, `Role.Mark(mark)` the key a style's
rule is by (classes `okay-mark okay-mark-price`). A mark changes nothing about the view within; it is the word a style
and a reader may act on. On the wire the mark is a string, as a value must be.

TYPED (2026-10-21, `Marks[M]` in core): the application says its marks as a TYPE, a sealed set of cases with a
`Shape` — Scala 3's `enum`, a sealed trait of case objects on 2.13 — and the strings are made for it:

```scala
sealed trait Mine; object Mine { case object Price extends Mine; case object DueDate extends Mine }
val marks = Marks.of(Shape.derived[Mine]).explained(Mine.Price -> "the amount due, in euro")
marks.mark(Mine.Price, View.text("12 €"))      // View.Marked("price", …): the case's name, kebab, as everywhere
marks.role(Mine.Price)                          // Role.Mark("price"), for style.adjust(…)
marks.on(view): Option[Mine]                    // the mark back as the type
marks.vocabulary                                // the marks as data, each with its meaning
```

One source for three things: the look (a rule by `marks.role`), the wire (the string in `ViewJson`), and WHAT A
READER IS TOLD — the `Vocabulary` is a list of `(name, doc)`, told to an agent in the MCP server's instructions and
served as the resource `face://marks`, served by REST at `GET base/marks` with a `marks` link in every resource, and
what makes the Markdown projection SHOW a mark (`**price:** 12 €`) instead of seeing through it. A mark the vocabulary
does not know stays transparent, so a face that is not told anything behaves as before. A `Vocabulary` may also be
written by hand, `Vocabulary("price" -> "the amount due")`, and `Marks.of(Shape.text)` is the untyped case.

QUALIFIED: two vocabularies in one application may collide on a word; `marks.qualified("shop")` says `shop.price` —
the view, the role and the vocabulary all carry the qualified name, the class is `okay-mark-shop.price`, and `Css`
escapes the dot in the selector. Optional, short, and the application's: the library does not invent namespaces.

## The interpreters

| Face | Interpreter | What a rule becomes |
|---|---|---|
| `html`, `web/laminar` | `Css(look)` in core; served at `GET /okay.css`, or `LaminarFace.stylesheet(look)` as `<style>` | custom properties `--okay-fg` … on `:root`; a block per role with a rule, in `var(--okay-…)` |
| `swing` | `Looks(look)(root)`, after each rebuild when `panel(…, SwingFace.Settings(look = Some(l)))` | `Font` (family, bold, size × base), `setForeground`, `setBackground` + opaque, `LineBorder` + `EmptyBorder` for pad |
| `term` | `Ansi(look): Paint`, given to `Terminal(faces, paint)` | SGR: `1` for bold, `38;2;r;g;b` for the tone, `48;2;r;g;b` for the background; reset after |

Rules of the interpreters:
1. A rule that says nothing produces nothing: no block, no font change, no escape. `Css` emits only the declarations the
   rule sets, so the stylesheet is readable and an application's own CSS has little to fight.
2. The stylesheet carries colours in the palette alone; every rule's block names tones by `var(--okay-…)`. A page may
   redefine the custom properties and the whole look follows.
3. The terminal paints ONLY when given a `Paint`: `Render.view(v)` and `Terminal(faces)` are plain, so pipes and tests
   stay clean. An application decides — `if (System.console() != null) Ansi(look) else Paint.none`.
4. Swing without a look is the platform's look and feel, untouched. With one, components with no role get the face's
   font and foreground so the look is whole.
5. `Css` keeps a small structural base that is not style — margins zeroed inside the face, `border-collapse`, a form
   and a field as columns, a button with no browser chrome — so the rules are the look and nothing else is.

## The HTML face's markup

Structure alone, HTML5 that is well-formed XML: every element closed, void elements self-closed (`<input …/>`),
attributes quoted, text escaped, no style inline, no script. `<main class="okay-face">` holds `<section
class="okay-view">`, `<ul class="okay-problems">` when there are problems, and `<nav class="okay-actions">` of forms.
Views are semantic tags with their role's classes: `p`, `h1`–`h3`, `dl` with `dt.okay-label`, `table`, `ul`,
`section` (a group; `section.okay-told` for what was told above the current view), `a`, `pre`, `span.okay-masked`.
The HTML suite checks every page it gets with a tag-balance checker (`WellFormed`).

## Tests

`core`: every role is found back by its class; the default style says something for the roles a dialogue is read by;
the stylesheet has a property per tone, a block per role with a rule, tones not colours in the blocks. `swing`: with
a look the heading is bold and 1.6 × the base, the problem in its tone, the face on its background, a button an accent;
without one the platform stands. `term`: SGR around the heading, the problem, a label, a menu entry; plain output has
no escape. `html`: the page links the stylesheet and has no `<style>`; `/okay.css` is served as `text/css`; a dark
look is another stylesheet. `web/laminar`: the `<style>` element from a look; the roles' classes on the elements.

## Not done, and why

- Dark mode: DONE as `Css(light, dark)` — one stylesheet, the light theme's tokens on `:root`, the dark theme's under
  `@media (prefers-color-scheme: dark)`; the rules are the light look's, and stand under both since they name tones.
- Per-application roles: DONE as marks, typed by `Marks[M]` — see "Marks" above. A mark with no rule is the view
  within, unmarked; a mark no vocabulary names is seen through by the readers.
- Fonts by name: `Family` is a kind, each face picks the face. A theme with a web font is an application's own CSS.
