package okay.face

import okay.effects.{Runner, Std, Sum, Void}
import okay.effects.machine.Machine
import okay.effects.parsing.Parsing

/** okay-simple's parsing module, a JSON grammar on it (the one in its docs/modules/parsing.md), AGREEING with the
 * hand-written `Json.parse`: the same `Value` where both parse, a failure where either fails. `Json.parse` is written
 * by hand for the module's missing positions in errors and its backtracking (okay-simple's BACKLOG.md, "parsing:") */
class JsonGrammarSuite extends munit.FunSuite {
  implicit val runner: Runner = Machine
  val ps = Parsing()
  import ps._
  import ps.fx._
  import Value._

  type P[X] = Sum[({ type L[x] = Sum[Void, Std.Choose, x] })#L, Input, X]

  def ws: Unit ! P = many[Char, P](satisfy[P](_.isWhitespace)).as(())
  def token(c: Char): Unit ! P = ws.flatMap(_ => char[P](c)).flatMap(_ => ws)
  def sepBy[A](p: A ! P, sep: Char): List[A] ! P =
    orElse[List[A], P](p.flatMap(a => many[A, P](token(sep).flatMap(_ => p)).map(a :: _)), pure[List[A], P](Nil))
  def digits: String ! P = many1[Char, P](satisfy[P](_.isDigit)).map(_.mkString)
  def optional(p: String ! P): String ! P = orElse[String, P](p, pure[String, P](""))
  def number: Value ! P =
    optional(char[P]('-').as("-")).flatMap(sign => digits.flatMap(int => optional(char[P]('.').flatMap(_ => digits).map("." + _)).flatMap(frac =>
      optional(satisfy[P](c => c == 'e' || c == 'E').flatMap(_ => optional(satisfy[P](c => c == '+' || c == '-').map(_.toString))).flatMap(s => digits.map("e" + s + _)))
        .map(exp => Num(BigDecimal(sign + int + frac + exp))))))
  def hex: Char ! P = satisfy[P](c => c.isDigit || ('a' to 'f').contains(c.toLower))
  def escape: Char ! P = char[P]('\\').flatMap(_ => item[P]).flatMap {
    case 'n' => pure[Char, P]('\n')
    case 't' => pure[Char, P]('\t')
    case 'r' => pure[Char, P]('\r')
    case 'b' => pure[Char, P]('\b')
    case 'f' => pure[Char, P]('\f')
    case 'u' => hex.flatMap(a => hex.flatMap(b => hex.flatMap(c => hex.map(d => Integer.parseInt(s"$a$b$c$d", 16).toChar))))
    case c => pure[Char, P](c)
  }
  def text: String ! P =
    char[P]('"').flatMap(_ => many[Char, P](orElse[Char, P](satisfy[P](c => c != '"' && c != '\\'), escape))).flatMap(cs => char[P]('"').as(cs.mkString))
  def literal(word: String, v: Value): Value ! P = string[P](word).as(v)
  def value: Value ! P = ws.flatMap(_ => orElse[Value, P](literal("null", Null), orElse[Value, P](literal("true", Bool(true)),
    orElse[Value, P](literal("false", Bool(false)), orElse[Value, P](number, orElse[Value, P](text.map(Str(_)),
    orElse[Value, P](array, obj))))))).flatMap(v => ws.as(v))
  def array: Value ! P = char[P]('[').flatMap(_ => sepBy(value, ',')).flatMap(items => token(']').as(Arr(items.toVector)))
  def obj: Value ! P =
    char[P]('{').flatMap(_ => sepBy(ws.flatMap(_ => text).flatMap(k => token(':').flatMap(_ => value).map(k -> _)), ',')).flatMap(fs => token('}').as(Obj(fs.toVector)))

  def onModule(s: String): Option[Value] = ps.fx.run(parse(s)(value))

  val texts: List[String] = List(
    """{"a": [1, -2.5e2, true], "b": {"c": null, "d": "x\"yé\n"}, "e": [], "f": {}}""",
    " [ ] ", "{}", "\"\"", "12", "-0.5", "1E+3", "null", " true ",
    Json.print(Schema.shape.encode(Schema.shape.erased)),
    "[1, 2", """{"a": }""", "tru", "{\"a\":1,}", "\"abc", "[1,,2]", "01x", "")

  test("the module's grammar and Json.parse agree on every text") {
    texts.foreach { t =>
      assertEquals(onModule(t), Json.parse(t).toOption, s"on: $t")
    }
  }
  test("and both parse what the other prints") {
    val v = Schema.shape.encode(Schema.shape.erased)
    assertEquals(onModule(Json.pretty(v)), Some(v))
    assertEquals(Json.parse(Json.print(v)), Right(v))
  }
}
