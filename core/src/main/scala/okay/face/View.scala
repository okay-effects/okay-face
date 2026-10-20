package okay.face

import okay.effects.json.Json
import okay.effects.shape.Schema

/**
 * WHAT IS SHOWN: a representation, semantic and nothing else — no layout, no colour, no widths, as semantic HTML or
 * Markdown are and CSS is not. A VALUE: no function inside, so it has equality and crosses a wire; and no secret the
 * program did not put there — `Masked` is how a secret is present without being shown. Every face draws it its way.
 */
sealed trait View
object View {
  case object Empty extends View
  /** a paragraph */
  final case class Text(s: String) extends View
  /** 1 to 3 */
  final case class Heading(level: Int, s: String) extends View
  final case class Notice(level: Notice.Level, s: String) extends View
  object Notice {
    sealed trait Level
    case object Info extends Level
    case object Warning extends Level
    case object Problem extends Level
  }
  /** a record shown: label to value */
  final case class Fields(rows: Vector[(String, View)]) extends View
  final case class Table(columns: Vector[String], rows: Vector[Vector[View]]) extends View
  /** a list */
  final case class Items(items: Vector[View]) extends View
  final case class Group(title: Option[String], children: Vector[View]) extends View
  /** names an entity; a face may make it a link */
  final case class Ref(key: String, label: String) extends View
  /** a structured value shown as it is, its schema if known */
  final case class Datum(json: Json, schema: Option[Schema] = None) extends View
  /** a secret NOT shown: "•••• 4821" */
  final case class Masked(hint: String) extends View

  def text(s: String): View = Text(s)
  def heading(s: String, level: Int = 1): View = Heading(level, s)
  def info(s: String): View = Notice(Notice.Info, s)
  def warning(s: String): View = Notice(Notice.Warning, s)
  def problem(s: String): View = Notice(Notice.Problem, s)
  def fields(rows: (String, View)*): View = Fields(rows.toVector)
  def table(columns: String*)(rows: Vector[View]*): View = Table(columns.toVector, rows.toVector)
  def items(xs: View*): View = Items(xs.toVector)
  def group(title: String)(children: View*): View = Group(Some(title), children.toVector)
  def stack(children: View*): View = Group(None, children.toVector)
}
