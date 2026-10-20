package okay.face

import okay.effects.{Sum, Void}
import okay.effects.json.Json
import okay.effects.machine.Machine
import okay.effects.shape.Shape
import okay.effects.shape.Shape.{field, int, record, unit}

/** a tiny domain effect described: served by name, performed from afar through a transport that serves it */
sealed trait Counter[X]
object Counter {
  final case class Get[X](answer: Int => X) extends Counter[X]
  final case class Add[X](n: Int, answer: Int => X) extends Counter[X]
  final case class Reset[X](answer: Unit => X) extends Counter[X]

  val get: Op[Counter, Unit, Int] = Op("get", unit, int, (_: Unit) => Get(x => x), Some("the count"))
  val add: Op[Counter, Int, Int] = Op("add", record(field("n", int))(n => n)(n => n), int, (n: Int) => Add(n, x => x), Some("count up by n"), writes = true)
  val reset: Op[Counter, Unit, Unit] = Op("reset", unit, unit, (_: Unit) => Reset(x => x), writes = true)
  val describe: Describe[Counter] = new Describe[Counter]("counter") {
    val operations = List(get, add, reset)
    def split[X](op: Counter[X]): Call[X] = op match {
      case Get(k) => call(get, (), k)
      case Add(n, k) => call(add, n, k)
      case Reset(k) => call(reset, (), k)
    }
  }
}

class DescribeSuite extends munit.FunSuite {
  import Machine._
  val domains: Domains[Machine.type] = new Domains[Machine.type]()(Machine)
  type C[X] = Sum[Void, Counter, X]

  /** the real handler: a cell */
  def counting[R[_], Rest[_], A](initial: Int)(p: A ! R)(implicit sp: Split[Counter, R, Rest]): (Int, A) ! Rest =
    handle[Counter, Int, R, Rest, A, (Int, A)](p, initial, new StatefulTailHandler[Counter, Int, Rest, A, (Int, A)] {
      def ret(s: Int, a: A) = pure((s, a))
      def answer[X](cell: Cell[Int], op: Counter[X]): X = op match {
        case Counter.Get(k) => k(cell.get)
        case Counter.Add(n, k) => cell.set(cell.get + n); k(cell.get)
        case Counter.Reset(k) => cell.set(0); k(())
      }
    })

  test("served by name: the parameters decoded, performed under the handler, the result encoded; the problems otherwise") {
    val (n, rs) = Machine.run(counting[C, Void, List[Either[okay.effects.shape.Problems, Json]]](1)(
      domains.serve[Counter, C](Counter.describe)("add", Json.obj("n" -> Json.num(2))).flatMap(a =>
        domains.serve[Counter, C](Counter.describe)("get", Json.Obj.empty).flatMap(g =>
          domains.serve[Counter, C](Counter.describe)("add", Json.obj("n" -> Json.Str("x"))).flatMap(bad =>
            domains.serve[Counter, C](Counter.describe)("nope", Json.Obj.empty).map(none => List(a, g, bad, none)))))))
    assertEquals(n, 3)
    assertEquals(rs.take(2), List(Right(Json.num(3)), Right(Json.num(3))))
    assert(rs(2).isLeft && rs(3).left.exists(_.head.path == List("operation")), rs.toString)
  }
  /** the service elsewhere: its own run, its own state, as a server's — the transport crosses to it */
  var state = 10
  def service(o: Op[Counter, _, _], args: Json): Json ! Void = {
    val (s, r) = Machine.run(counting[C, Void, Either[okay.effects.shape.Problems, Json]](state)(domains.serve[Counter, C](Counter.describe)(o.name, args)))
    state = s
    pure[Json, Void](r.fold(ps => throw new IllegalStateException(ps.toString), j => j))
  }
  test("performed from afar: the program's effect split, carried, served, its answer decoded") {
    val program: Int ! C = perform[Counter].apply[C, Int](Counter.Add(2, x => x)).flatMap(_ => perform[Counter].apply[C, Int](Counter.Add(3, x => x))).flatMap(_ =>
      perform[Counter].apply[C, Unit](Counter.Reset(x => x))).flatMap(_ => perform[Counter].apply[C, Int](Counter.Add(4, x => x)))
    var carried: List[(String, Json)] = Nil
    val transport: (Op[Counter, _, _], Json) => Json ! Void = (o, args) => { carried = (o.name, args) :: carried; service(o, args) }
    state = 10
    val a = Machine.run(domains.remote[Counter](Counter.describe).apply[C, Void, Int](transport)(program))
    assertEquals((state, a), (4, 4))
    assertEquals(carried.reverse.map(_._1), List("add", "add", "reset", "add"))
    assertEquals(carried.reverse.head._2, Json.obj("n" -> Json.num(2)))
  }
  test("an answer that does not decode is a RemoteFailure, not a problem") {
    val program: Int ! C = perform[Counter].apply[C, Int](Counter.Get(x => x))
    val transport: (Op[Counter, _, _], Json) => Json ! Void = (_, _) => pure[Json, Void](Json.Str("not a number"))
    intercept[RemoteFailure](Machine.run(domains.remote[Counter](Counter.describe).apply[C, Void, Int](transport)(program)))
  }
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
