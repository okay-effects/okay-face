package okay.face.rest

import okay.effects.{Sum, Void}
import okay.effects.json.Json
import okay.effects.machine.Machine
import okay.effects.shape.Shape.{field, int, record, text, unit}
import okay.face.{Call, Describe, Op}
import okay.http.{Fetch, Https, Request, Response}

/** a described effect on REST: served as resources, performed from afar over Fetch against them — in one process */
sealed trait Notes[X]
object Notes {
  final case class Find[X](text: String, limit: Option[Int], answer: List[String] => X) extends Notes[X]
  final case class Put[X](note: String, answer: Int => X) extends Notes[X]
  val find: Op[Notes, (String, Option[Int]), List[String]] =
    Op("find", record(field("text", text), field("limit", int.optional))((t, l) => (t, l))(x => x), text.many(), (a: (String, Option[Int])) => Find(a._1, a._2, x => x), Some("notes with the text"))
  val put: Op[Notes, String, Int] = Op("put", record(field("note", text))(n => n)(n => n), int, (n: String) => Put(n, x => x), Some("a note kept; how many there are"), writes = true)
  val describe: Describe[Notes] = new Describe[Notes]("notes") {
    val operations = List(find, put)
    def split[X](op: Notes[X]): Call[X] = op match {
      case Find(t, l, k) => call(find, (t, l), k)
      case Put(n, k) => call(put, n, k)
    }
  }
}

class RestDomainsSuite extends munit.FunSuite {
  import Machine._
  val domains: RestDomains[Machine.type] = new RestDomains[Machine.type]()(Machine)
  val https: Https[Machine.type] = new Https[Machine.type]()(Machine)
  type N[X] = Sum[Void, Notes, X]
  type F[X] = Sum[Void, Fetch, X]
  type NF[X] = Sum[F, Notes, X]   // the program: Notes performed here, carried by Fetch

  def keeping[R[_], Rest[_], A](initial: List[String])(p: A ! R)(implicit sp: Split[Notes, R, Rest]): (List[String], A) ! Rest =
    handle[Notes, List[String], R, Rest, A, (List[String], A)](p, initial, new StatefulTailHandler[Notes, List[String], Rest, A, (List[String], A)] {
      def ret(s: List[String], a: A) = pure((s, a))
      def answer[X](cell: Cell[List[String]], op: Notes[X]): X = op match {
        case Notes.Find(t, l, k) => val found = cell.get.filter(_.contains(t)); k(l.fold(found)(found.take))
        case Notes.Put(n, k) => cell.set(cell.get :+ n); k(cell.get.size)
      }
    })
  /** the server: its own state, each request its own run */
  var notes: List[String] = Nil
  val served = domains.serve[Notes, N](Notes.describe, "/notes")
  def server(req: Request): Response = { val (s, r) = Machine.run(keeping[N, Void, Response](notes)(served.service(req))); notes = s; r }

  test("the description, a GET with the query, a POST with JSON, a 400 with the problems, a 405") {
    notes = List("tea", "coffee")
    val d = server(Request.get("/notes"))
    assertEquals(d.status, 200)
    assert(d.body.contains("\"rel\":\"find\"") && d.body.contains("\"method\":\"GET\"") && d.body.contains("\"rel\":\"put\"") && d.body.contains("\"method\":\"POST\""), d.body)
    assertEquals(server(Request.get("/notes/find?text=e&limit=1")).body, "[\"tea\"]")
    assertEquals(server(Request.get("/notes/find?text=e")).body, "[\"tea\",\"coffee\"]")
    assertEquals(server(Request.json("POST", "/notes/put", Json.obj("note" -> Json.Str("milk")))).body, "3")
    val bad = server(Request.json("POST", "/notes/put", Json.obj("nope" -> Json.Str("x"))))
    assertEquals(bad.status, 400); assert(bad.body.contains("note"), bad.body)
    assertEquals(server(Request.get("/notes/put")).status, 405)
  }
  test("performed from afar: the program's Notes over Fetch against the server; a 400 is a RemoteFailure") {
    notes = Nil
    val program: List[String] ! NF = perform[Notes].apply[NF, Int](Notes.Put("tea", x => x)).flatMap(_ => perform[Notes].apply[NF, Int](Notes.Put("tee time", x => x))).flatMap(_ =>
      perform[Notes].apply[NF, List[String]](Notes.Find("te", Some(5), x => x)))
    val fetched = new TailHandler[Fetch, Void, List[String], List[String]] {
      def ret(a: List[String]) = pure(a)
      def answer[X](op: Fetch[X]): X = op match {
        case Fetch.Call(m, url, hs, body, k) => k(server(Request(m, url).copy(headers = hs.map { case (h, v) => h.toLowerCase -> List(v) }, body = body)))
      }
    }
    val found = Machine.run(handle[Fetch, F, Void, List[String], List[String]](domains.remote[Notes](Notes.describe, "/notes").apply[NF, F, List[String]](program), fetched))
    assertEquals(found, List("tea", "tee time"))
    assertEquals(notes, List("tea", "tee time"))
  }
}
