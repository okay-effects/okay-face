package okay.face

import Value._

/** JSON text of a `Value`, both ways — our own hundred lines, so that no face needs a library for it. A damaged text
 * is a `Problem` with its offset, never a throw */
object Json {
  def print(v: Value): String = { val b = new StringBuilder; write(v, b, -1, 0); b.toString }
  def pretty(v: Value, indent: Int = 2): String = { val b = new StringBuilder; write(v, b, indent, 0); b.toString }

  private def write(v: Value, b: StringBuilder, indent: Int, depth: Int): Unit = v match {
    case Null => b.append("null")
    case Bool(x) => b.append(if (x) "true" else "false")
    case Num(n) => b.append(n.bigDecimal.toPlainString)
    case Str(s) => quote(s, b)
    case Arr(items) =>
      if (items.isEmpty) b.append("[]")
      else {
        b.append('['); var first = true
        items.foreach { x => if (!first) b.append(','); first = false; newline(b, indent, depth + 1); write(x, b, indent, depth + 1) }
        newline(b, indent, depth); b.append(']')
      }
    case Obj(fields) =>
      if (fields.isEmpty) b.append("{}")
      else {
        b.append('{'); var first = true
        fields.foreach { case (k, x) =>
          if (!first) b.append(','); first = false; newline(b, indent, depth + 1)
          quote(k, b); b.append(':'); if (indent >= 0) b.append(' '); write(x, b, indent, depth + 1)
        }
        newline(b, indent, depth); b.append('}')
      }
  }
  private def newline(b: StringBuilder, indent: Int, depth: Int): Unit =
    if (indent >= 0) { b.append('\n'); var i = 0; while (i < indent * depth) { b.append(' '); i += 1 } }
  private def quote(s: String, b: StringBuilder): Unit = {
    b.append('"')
    s.foreach {
      case '"' => b.append("\\\"")
      case '\\' => b.append("\\\\")
      case '\n' => b.append("\\n")
      case '\r' => b.append("\\r")
      case '\t' => b.append("\\t")
      case '\b' => b.append("\\b")
      case '\f' => b.append("\\f")
      case c if c < 0x20 => b.append(f"\\u${c.toInt}%04x")
      case c => b.append(c)
    }
    b.append('"')
  }

  def parse(s: String): Either[Problem, Value] = {
    val p = new Parser(s)
    try {
      p.ws(); val v = p.value(); p.ws()
      if (p.i < s.length) Left(p.problem("text after the value")) else Right(v)
    } catch { case Fail(problem) => Left(problem) }
  }

  private final case class Fail(problem: Problem) extends RuntimeException(problem.message, null, false, false)

  private final class Parser(s: String) {
    var i = 0
    def problem(msg: String): Problem = Problem(Nil, s"at offset $i: $msg")
    def fail(msg: String): Nothing = throw Fail(problem(msg))
    def ws(): Unit = while (i < s.length && (s.charAt(i) == ' ' || s.charAt(i) == '\n' || s.charAt(i) == '\r' || s.charAt(i) == '\t')) i += 1
    def peek: Char = if (i < s.length) s.charAt(i) else fail("the text ends too soon")
    def expect(c: Char): Unit = if (peek == c) i += 1 else fail(s"expected '$c'")
    def word(w: String, v: Value): Value = if (s.startsWith(w, i)) { i += w.length; v } else fail(s"expected $w")
    def value(): Value = peek match {
      case '{' => obj()
      case '[' => arr()
      case '"' => Str(string())
      case 't' => word("true", Bool(true))
      case 'f' => word("false", Bool(false))
      case 'n' => word("null", Null)
      case c if c == '-' || (c >= '0' && c <= '9') => number()
      case c => fail(s"unexpected '$c'")
    }
    def obj(): Value = {
      expect('{'); ws(); val fields = Vector.newBuilder[(String, Value)]
      if (peek == '}') { i += 1; return Obj(fields.result()) }
      while (true) {
        ws(); if (peek != '"') fail("expected a field name")
        val k = string(); ws(); expect(':'); ws(); val v = value(); fields += (k -> v); ws()
        peek match {
          case ',' => i += 1
          case '}' => i += 1; return Obj(fields.result())
          case _ => fail("expected ',' or '}'")
        }
      }
      Obj(fields.result())
    }
    def arr(): Value = {
      expect('['); ws(); val items = Vector.newBuilder[Value]
      if (peek == ']') { i += 1; return Arr(items.result()) }
      while (true) {
        ws(); items += value(); ws()
        peek match {
          case ',' => i += 1
          case ']' => i += 1; return Arr(items.result())
          case _ => fail("expected ',' or ']'")
        }
      }
      Arr(items.result())
    }
    def string(): String = {
      expect('"'); val b = new StringBuilder
      while (true) {
        val c = peek; i += 1
        c match {
          case '"' => return b.toString
          case '\\' =>
            val e = peek; i += 1
            e match {
              case '"' => b.append('"'); case '\\' => b.append('\\'); case '/' => b.append('/')
              case 'n' => b.append('\n'); case 'r' => b.append('\r'); case 't' => b.append('\t')
              case 'b' => b.append('\b'); case 'f' => b.append('\f')
              case 'u' =>
                if (i + 4 > s.length) fail("a \\u escape needs four hex digits")
                val hex = s.substring(i, i + 4)
                try b.append(Integer.parseInt(hex, 16).toChar) catch { case _: NumberFormatException => fail(s"bad \\u escape '$hex'") }
                i += 4
              case other => fail(s"bad escape '\\$other'")
            }
          case c if c < 0x20 => fail("a control character in a string")
          case c => b.append(c)
        }
      }
      b.toString
    }
    def number(): Value = {
      val start = i
      if (peek == '-') i += 1
      while (i < s.length && s.charAt(i) >= '0' && s.charAt(i) <= '9') i += 1
      if (i < s.length && s.charAt(i) == '.') { i += 1; while (i < s.length && s.charAt(i) >= '0' && s.charAt(i) <= '9') i += 1 }
      if (i < s.length && (s.charAt(i) == 'e' || s.charAt(i) == 'E')) {
        i += 1; if (i < s.length && (s.charAt(i) == '+' || s.charAt(i) == '-')) i += 1
        while (i < s.length && s.charAt(i) >= '0' && s.charAt(i) <= '9') i += 1
      }
      val text = s.substring(start, i)
      try Num(BigDecimal(text)) catch { case _: NumberFormatException => fail(s"not a number: '$text'") }
    }
  }
}
