package okay.faces

import okay.effects.json.Json
import okay.effects.shape.Shape

sealed trait Mine
object Mine {
  case object Price extends Mine
  case object DueDate extends Mine
  val shape: Shape[Mine] = Shape.derived[Mine]
}

/** THE MARKS TYPED: an application's sealed set with a shape names the marks, keys the rules, is read back and told */
class MarksSuite extends munit.FunSuite {
  val marks = Marks.of(Mine.shape).explained(Mine.Price -> "the amount due, in euro")
  test("a case is a mark by its case name; the view and the role carry the string; the mark comes back as the type") {
    assertEquals(marks.name(Mine.DueDate), "due-date")
    assertEquals(marks.mark(Mine.Price, View.text("12")), View.Marked("price", View.text("12")))
    assertEquals(marks.role(Mine.Price), Role.Mark("price"))
    assertEquals(marks.on(View.Marked("price", View.text("12"))), Some(Mine.Price))
    assertEquals(marks.on(View.Marked("other", View.text("12"))), None)
    assertEquals(marks.of("due-date"), Some(Mine.DueDate))
    assertEquals(marks.names, List("price", "due-date"))
  }
  test("the vocabulary: every mark, its meaning when explained, as JSON and Markdown; decoded back") {
    val v = marks.vocabulary
    assertEquals(v.entries,
      List(Vocabulary.Entry("price", Some("the amount due, in euro")), Vocabulary.Entry("due-date", None)))
    assertEquals(v.markdown, "- price: the amount due, in euro\n- due-date")
    assertEquals(Vocabulary.decode(v.json), Right(v))
    assertEquals(Json.print(v.json), """[{"name":"price","doc":"the amount due, in euro"},{"name":"due-date"}]""")
    assertEquals((v ++ Vocabulary("price" -> "again", "badge" -> "a badge")).entries.map(_.name),
      List("price", "due-date", "badge"))
  }
  test("qualified: two vocabularies apart; the class keeps the dot, the stylesheet escapes it") {
    val shop = marks.qualified("shop")
    assertEquals(shop.name(Mine.Price), "shop.price")
    assertEquals(shop.of("price"), None)
    assertEquals(shop.on(View.Marked("shop.price", View.Empty)), Some(Mine.Price))
    assertEquals(shop.role(Mine.Price).cls, "okay-mark-shop.price")
    assertEquals(Role.ofClass("okay-mark-shop.price"), Some(Role.Mark("shop.price")))
    val css = Css(Look(Theme.light, Style.plain.adjust(shop.role(Mine.Price))(_.copy(bold = true))))
    assert(css.contains(".okay-mark-shop\\.price{font-weight:bold}"), css)
    assertEquals(shop.vocabulary.entries.map(_.name), List("shop.price", "shop.due-date"))
  }
  test("a Shape[String] is the untyped vocabulary: the string is the word") {
    val free = Marks.of(Shape.text)
    assertEquals(free.name("badge"), "badge"); assertEquals(free.of("badge"), Some("badge"));
    assertEquals(free.vocabulary, Vocabulary.empty)
  }
}
