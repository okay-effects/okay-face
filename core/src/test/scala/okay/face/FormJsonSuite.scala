package okay.face

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
}
