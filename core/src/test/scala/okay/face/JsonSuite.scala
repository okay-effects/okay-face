package okay.face

import Value._

class JsonSuite extends munit.FunSuite {
  val sample: Value = obj("name" -> Str("Ann \"the\" Fast\n"), "age" -> num(41), "tags" -> arr(Str("a"), Str("b")),
    "price" -> Num(BigDecimal("12.50")), "none" -> Null, "ok" -> Bool(true), "nested" -> obj("deep" -> arr()))

  test("print and parse round trip") {
    assertEquals(Json.parse(Json.print(sample)), Right(sample))
    assertEquals(Json.parse(Json.pretty(sample)), Right(sample))
  }
  test("compact text is as expected") {
    assertEquals(Json.print(obj("a" -> num(1), "b" -> arr(Str("x"), Null))), """{"a":1,"b":["x",null]}""")
    assertEquals(Json.print(Num(BigDecimal("1E+3"))), "1000")
  }
  test("pretty text is indented") {
    assertEquals(Json.pretty(obj("a" -> num(1), "b" -> arr(Str("x")))), "{\n  \"a\": 1,\n  \"b\": [\n    \"x\"\n  ]\n}")
  }
  test("escapes both ways") {
    val s = Str("tab\t nl\n quote\" back\\ bell\u0007 é ✓")
    assertEquals(Json.parse(Json.print(s)), Right(s))
    assertEquals(Json.parse("\"\\u00e9\\u2713\""), Right(Str("é✓")))
  }
  test("numbers") {
    assertEquals(Json.parse("-12.5e2"), Right(Num(BigDecimal("-12.5e2"))))
    assertEquals(Json.parse("0"), Right(num(0)))
  }
  test("damaged text is a problem with an offset, never a throw") {
    assert(Json.parse("{\"a\": }").isLeft)
    assert(Json.parse("[1, 2").left.exists(_.message.contains("ends too soon")))
    assert(Json.parse("{} x").left.exists(_.message.contains("text after")))
    assert(Json.parse("\"abc").isLeft)
    assert(Json.parse("tru").isLeft)
    assert(Json.parse("{\"a\":1,}").isLeft)
  }
  test("whitespace anywhere") {
    assertEquals(Json.parse(" { \"a\" : [ 1 , 2 ] , \"b\" : { } } "), Right(obj("a" -> arr(num(1), num(2)), "b" -> Obj.empty)))
  }
}
