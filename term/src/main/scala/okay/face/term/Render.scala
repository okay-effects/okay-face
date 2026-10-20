package okay.face.term

import okay.effects.json.Json
import okay.face.View

/** A VIEW AS TEXT for a terminal: deterministic, pipe-friendly, no colour. Lines, 78 columns where text wraps */
object Render {
  val width = 78

  def view(v: View): List[String] = v match {
    case View.Empty => Nil
    case View.Text(s) => wrap(s)
    case View.Heading(1, s) => List(s.toUpperCase, "=" * s.length)
    case View.Heading(2, s) => List(s, "-" * s.length)
    case View.Heading(_, s) => List("## " + s)
    case View.Notice(level, s) =>
      val prefix = level match { case View.Notice.Info => "info: "; case View.Notice.Warning => "warning: "; case View.Notice.Problem => "problem: " }
      wrap(prefix + s)
    case View.Fields(rows) =>
      val w = if (rows.isEmpty) 0 else rows.map(_._1.length).max
      rows.toList.map { case (label, value) => label.padTo(w, ' ') + ": " + oneLine(value) }
    case View.Table(columns, rows) =>
      val cells = rows.map(_.map(oneLine))
      val widths = columns.indices.map(i => (columns(i).length +: cells.map(r => if (i < r.size) r(i).length else 0)).max)
      def line(cs: Vector[String]) = cs.zipWithIndex.map { case (c, i) => c.padTo(widths(i), ' ') }.mkString("  ").replaceAll("\\s+$", "")
      val header = if (columns.forall(_.isEmpty)) Nil else List(line(columns), widths.map("-" * _).mkString("  "))
      header ++ cells.toList.map(line)
    case View.Items(items) => items.toList.flatMap(i => view(i) match { case Nil => Nil; case first :: rest => ("- " + first) :: rest.map("  " + _) })
    case View.Group(title, children) =>
      val body = children.toList.flatMap(view)
      title.fold(body)(t => ("## " + t) :: body.map("  " + _))
    case View.Ref(key, label) => List(s"$label [$key]")
    case View.Datum(json, _) => Json.pretty(json).split("\n").toList
    case View.Masked(hint) => List(hint)
  }

  /** a view in one line, for a cell or a field's value */
  def oneLine(v: View): String = v match {
    case View.Empty => ""
    case View.Text(s) => s
    case View.Heading(_, s) => s
    case View.Notice(_, s) => s
    case View.Fields(rows) => rows.map { case (l, x) => s"$l: ${oneLine(x)}" }.mkString(", ")
    case View.Table(_, rows) => rows.map(_.map(oneLine).mkString(" ")).mkString(" / ")
    case View.Items(items) => items.map(oneLine).mkString(", ")
    case View.Group(title, children) => (title.toList ++ children.map(oneLine)).mkString(" ")
    case View.Ref(key, label) => s"$label [$key]"
    case View.Datum(json, _) => Json.print(json)
    case View.Masked(hint) => hint
  }

  def wrap(s: String, w: Int = width): List[String] = s.split("\n", -1).toList.flatMap { para =>
    if (para.length <= w) List(para)
    else para.split(" ").foldLeft(List.empty[String]) {
      case (Nil, word) => List(word)
      case (line :: rest, word) => if (line.length + 1 + word.length <= w) (line + " " + word) :: rest else word :: line :: rest
    }.reverse
  }
}
