package okay.face

/** THE NEUTRAL DATUM: JSON-shaped on purpose. Every face and every transport speaks `Value`; a typed value exists only
 * inside a program, after a `Shape` decoded it. An `Obj` is ORDERED: the order a form shows, a wire writes */
sealed trait Value
object Value {
  case object Null extends Value
  final case class Bool(b: Boolean) extends Value
  final case class Num(n: BigDecimal) extends Value
  final case class Str(s: String) extends Value
  final case class Arr(items: Vector[Value]) extends Value
  final case class Obj(fields: Vector[(String, Value)]) extends Value {
    def get(name: String): Option[Value] = fields.collectFirst { case (n, v) if n == name => v }
    def has(name: String): Boolean = fields.exists(_._1 == name)
    def without(name: String): Obj = Obj(fields.filterNot(_._1 == name))
    def +:(field: (String, Value)): Obj = Obj(field +: fields)
  }
  object Obj {
    val empty: Obj = Obj(Vector.empty)
    def of(fields: (String, Value)*): Obj = Obj(fields.toVector)
  }

  def num(i: Int): Value = Num(BigDecimal(i))
  def num(l: Long): Value = Num(BigDecimal(l))
  def num(n: BigInt): Value = Num(BigDecimal(n))
  def str(s: String): Value = Str(s)
  def bool(b: Boolean): Value = Bool(b)
  def arr(items: Value*): Value = Arr(items.toVector)
  def obj(fields: (String, Value)*): Obj = Obj(fields.toVector)

  /** a word for what a value is, for a problem's message */
  def kind(v: Value): String = v match {
    case Null => "null"
    case Bool(_) => "a boolean"
    case Num(_) => "a number"
    case Str(_) => "text"
    case Arr(_) => "a list"
    case Obj(_) => "a record"
  }
}
