package okay.faces

import okay.effects.shape.Shape
import okay.effects.shape.Shape.{field, record, unit}

/** a form's fields and a query's, along a schema, both ways */
class FormJsonSuite extends munit.FunSuite {
  test("a form and a query along a schema, both ways: records, optionals, lists repeated, sums") {
    import okay.effects.shape.Shape.{sub, sum, text}
    final case class Q(text: String, tag: Option[String], tags: List[String])
    val q: Shape[Q] = record(field("text", text), field("tag", text.optional), field("tags", text.many()))(Q(_, _, _))(x => (x.text, x.tag, x.tags))
    val j = q.encode(Q("a b", None, List("x", "y")))
    val form = FormJson.print(q.erased, j)
    assertEquals(form, Map("text" -> List("a b"), "tags" -> List("x", "y")))
    assertEquals(q.decode(FormJson.collect(q.erased, form)), Right(Q("a b", None, List("x", "y"))))
    assertEquals(q.decode(FormJson.collect(q.erased, Map("text" -> List("t"), "tags" -> List("x\ny")))), Right(Q("t", None, List("x", "y"))))
    sealed trait Pet; final case class Dog(name: String) extends Pet; case object Fish extends Pet
    val pet: Shape[Pet] = sum(sub[Pet, Dog]("dog", record(field("name", text))(Dog(_))(_.name)) { case d: Dog => Some(d); case _ => None },
      sub[Pet, Fish.type]("fish", unit.map(_ => Fish)(_ => ())) { case Fish => Some(Fish); case _ => None })
    val pj = pet.encode(Dog("Rex"))
    val pf = FormJson.print(pet.erased, pj)
    assertEquals(pf, Map("type" -> List("dog"), "dog.name" -> List("Rex")))
    assertEquals(pet.decode(FormJson.collect(pet.erased, pf)), Right(Dog("Rex")))
  }
  test("a list of records: items by index, a blank slot no item, both ways") {
    final case class Line(what: String, qty: Int)
    final case class Order(who: String, lines: List[Line])
    val line = record(field("what", Shape.text), field("qty", Shape.int))(Line(_, _))(l => (l.what, l.qty))
    val order: Shape[Order] = record(field("who", Shape.text), field("lines", line.many(min = 1)))(Order(_, _))(o => (o.who, o.lines))
    val form = Map("who" -> List("ann"), "lines.0.what" -> List("tea"), "lines.0.qty" -> List("2"), "lines.1.what" -> List(""), "lines.1.qty" -> List(""), "lines.2.what" -> List("milk"), "lines.2.qty" -> List("1"))
    assertEquals(order.decode(FormJson.collect(order.erased, form)), Right(Order("ann", List(Line("tea", 2), Line("milk", 1)))))
    assertEquals(FormJson.print(order.erased, order.encode(Order("bo", List(Line("x", 1))))), Map("who" -> List("bo"), "lines.0.what" -> List("x"), "lines.0.qty" -> List("1")))
    assertEquals(order.decode(FormJson.collect(order.erased, Map("who" -> List("cy")))).left.map(_.map(_.render)), Left(List("lines: required")))
  }
}
