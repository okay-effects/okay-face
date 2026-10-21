package okay.faces

import okay.effects.json.Json
import okay.effects.shape.Shape

/**
 * AN APPLICATION'S MARKS, SAID ONCE: `View.Marked(mark, _)` carries a string, as a value crossing a wire must; the
 * application says its marks as a TYPE and has the string made for it. A sealed set of cases — Scala 3's `enum`, a
 * sealed trait of case objects — with a `Shape` (derived) is the vocabulary: `marks.mark(Mine.Price, view)` is the
 * view marked `"price"`, `marks.role(Mine.Price)` the key for a style's rule, `marks.on(view)` the mark back as the
 * type. One source for the look, the wire and what an agent is told: `vocabulary` is the marks as data, each with its
 * meaning when `explained`. A qualifier keeps two vocabularies apart when their words collide: `qualified("shop")`
 * says `"shop.price"`, the class `okay-mark-shop.price`.
 */
final class Marks[M](val shape: Shape[M], val qualifier: Option[String] = None, val meanings: Map[String, String] = Map.empty) {
  private val prefix: String = qualifier.fold("")(_ + ".")
  private val cases: List[String] = shape match { case Shape.OneOf(cs) => cs.map(_.name); case _ => Nil }
  /** the bare word of a mark, by its shape: a case's name, or the string itself for a `Shape[String]` */
  def word(m: M): String = shape.encode(m) match {
    case o: Json.Obj => o.get("type") match { case Some(Json.Str(t)) => t; case _ => throw new IllegalArgumentException(s"not a mark: $m") }
    case Json.Str(s) => s
    case other => throw new IllegalArgumentException(s"not a mark: $m encodes as ${Json.print(other)}")
  }
  /** the mark's name as the view and the style carry it: the word, qualified */
  def name(m: M): String = prefix + word(m)
  /** a mark back from its name, if it is one of these */
  def of(name: String): Option[M] =
    if (!name.startsWith(prefix)) None
    else { val bare = name.drop(prefix.length); (if (cases.nonEmpty) shape.decode(Json.obj("type" -> Json.Str(bare))) else shape.decode(Json.Str(bare))).toOption }
  def mark(m: M, view: View): View = View.Marked(name(m), view)
  def role(m: M): Role.Mark = Role.Mark(name(m))
  /** the mark on a view, when the view is marked with one of these */
  def on(view: View): Option[M] = view match { case View.Marked(n, _) => of(n); case _ => None }
  /** every mark's name, in the order of the cases */
  def names: List[String] = cases.map(prefix + _)
  def qualified(q: String): Marks[M] = new Marks[M](shape, Some(q), meanings)
  /** what a mark means, for whoever reads the vocabulary */
  def explained(pairs: (M, String)*): Marks[M] = new Marks[M](shape, qualifier, meanings ++ pairs.map { case (m, d) => word(m) -> d })
  /** the marks as data: names and meanings */
  def vocabulary: Vocabulary = Vocabulary(cases.map(c => Vocabulary.Entry(prefix + c, meanings.get(c))))
}
object Marks {
  def apply[M](implicit shape: Shape[M]): Marks[M] = new Marks[M](shape)
  def of[M](shape: Shape[M]): Marks[M] = new Marks[M](shape)
}

/** THE MARKS AS DATA: what an application's words are and mean — told to an agent as a resource and in the server's
 * instructions, served by REST, and what makes a Markdown face show a mark rather than see through it */
final case class Vocabulary(entries: List[Vocabulary.Entry]) {
  def isEmpty: Boolean = entries.isEmpty
  def has(name: String): Boolean = entries.exists(_.name == name)
  def doc(name: String): Option[String] = entries.find(_.name == name).flatMap(_.doc)
  def ++(other: Vocabulary): Vocabulary = Vocabulary(entries ++ other.entries.filterNot(e => has(e.name)))
  def json: Json = Json.Arr(entries.map(e => Json.obj(("name" -> Json.Str(e.name)) :: e.doc.map(d => "doc" -> Json.Str(d)).toList: _*)).toVector)
  /** one line per mark: `- price: the amount due` */
  def markdown: String = entries.map(e => "- " + e.name + e.doc.fold("")(": " + _)).mkString("\n")
}
object Vocabulary {
  final case class Entry(name: String, doc: Option[String] = None)
  val empty: Vocabulary = Vocabulary(Nil)
  /** by name, with meanings: `Vocabulary("price" -> "the amount due")` */
  def apply(pairs: (String, String)*): Vocabulary = Vocabulary(pairs.toList.map { case (n, d) => Entry(n, Some(d)) })
  def of(names: String*): Vocabulary = Vocabulary(names.toList.map(n => Entry(n)))
  def decode(j: Json): Either[String, Vocabulary] = j match {
    case Json.Arr(es) => Right(Vocabulary(es.toList.flatMap { case o: Json.Obj => o.get("name").collect { case Json.Str(n) => Entry(n, o.get("doc").collect { case Json.Str(d) => d }) }; case _ => None }))
    case _ => Left("a vocabulary is an array")
  }
}
