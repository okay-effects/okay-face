package okay.face.mcp

import okay.effects.{Sum, Void}
import okay.effects.json.Json
import okay.effects.machine.Machine
import okay.effects.shape.Shape.{field, int, record, unit}
import okay.face.{Call, Describe, Faces, Op}
import okay.mcp.ToolCall

/** a described effect as tools: listed with schemas, called by name, the result as JSON; and beside a dialogue's tools */
sealed trait Tally[X]
object Tally {
  final case class Get[X](answer: Int => X) extends Tally[X]
  final case class Add[X](n: Int, answer: Int => X) extends Tally[X]
  val get: Op[Tally, Unit, Int] = Op("get", unit, int, (_: Unit) => Get(x => x), Some("the tally"))
  val add: Op[Tally, Int, Int] = Op("add", record(field("n", int))(n => n)(n => n), int, (n: Int) => Add(n, x => x), Some("up by n"), writes = true)
  val describe: Describe[Tally] = new Describe[Tally]("tally") {
    val operations = List(get, add)
    def split[X](op: Tally[X]): Call[X] = op match { case Get(k) => call(get, (), k); case Add(n, k) => call(add, n, k) }
  }
}

class DomainToolsSuite extends munit.FunSuite {
  val faces: Faces[Machine.type] = new Faces[Machine.type]()(Machine)
  val face = McpFace(faces)
  import Machine._
  type T[X] = Sum[Void, Tally, X]
  def tallying[R[_], Rest[_], A](initial: Int)(p: A ! R)(implicit sp: Split[Tally, R, Rest]): (Int, A) ! Rest =
    handle[Tally, Int, R, Rest, A, (Int, A)](p, initial, new StatefulTailHandler[Tally, Int, Rest, A, (Int, A)] {
      def ret(s: Int, a: A) = pure((s, a))
      def answer[X](cell: Cell[Int], op: Tally[X]): X = op match { case Tally.Get(k) => k(cell.get); case Tally.Add(n, k) => cell.set(cell.get + n); k(cell.get) }
    })

  test("the tools, prefixed, with input and output schemas; a call performed, a bad call an error") {
    val tools = face.domain[Tally, T](Tally.describe)
    val (n, (specs, r1, r2)) = Machine.run(tallying[T, Void, (List[okay.mcp.ToolSpec], okay.mcp.ToolResult, okay.mcp.ToolResult)](5)(
      tools.list.flatMap(ss => tools.call(ToolCall("tally_add", Json.obj("n" -> Json.num(2)))).flatMap(a => tools.call(ToolCall("tally_add", Json.obj("n" -> Json.Str("x")))).map(b => (ss, a, b))))))
    assertEquals(specs.map(_.name), List("tally_get", "tally_add"))
    assert(specs(1).inputSchema.toString.contains("n") && specs(1).outputSchema.isDefined)
    assertEquals(n, 7); assertEquals(r1.structured, Some(Json.num(7))); assert(r2.isError)
  }
}
