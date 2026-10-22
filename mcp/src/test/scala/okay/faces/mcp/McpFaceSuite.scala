package okay.faces.mcp

import okay.effects.{Sum, Void}
import okay.effects.json.Json
import okay.effects.json.Json.{Str, num, obj}
import okay.effects.machine.Machine
import okay.effects.shape.Shape
import okay.effects.shape.Shape.{field, int, record, text}
import okay.faces.{Face, Faces, View, Vocabulary}
import okay.mcp.{Link, Rpc, Servers}

/** THE MCP FACE over a scripted link: the agent sees the actions as tools, calls one, is told list_changed, is refused
 * a
 * bad argument with the step unchanged, goes back */
class McpFaceSuite extends munit.FunSuite {
  val faces: Faces[Machine.type] = new Faces[Machine.type]()(Machine)
  val face = new McpFace[Machine.type]()(Machine)
  val servers: face.servers.type = face.servers
  import Machine._
  import faces._
  type L[X] = Sum[Void, Link, X]
  type F[X] = Sum[L, Face, X]

  final case class Who(name: String, age: Int)
  val who: Shape[Who] =
    record(field("name", text).labelled("Name"), field("age", int.check("of age")(_ >= 18)).documented("in years"))(
      Who(_, _))(w => (w.name, w.age))
  def greeting: String ! F =
    fill[Who, F](View.heading("Who are you?"), who, okay.faces.Step.Options.returnable).flatMap { w =>
      confirm[F](View.text(s"Greet ${w.name}, ${w.age}?")).flatMap { yes =>
        if (yes) show[F](View.text(s"Hello, ${w.name}!")).map(_ => s"greeted ${w.name}") else pure[String, F]("left")
      }
    }
  def line(m: Rpc): String = Rpc.encode(m)
  def rpc(l: String): Rpc = Rpc.decode(l).fold(f => f, m => m)
  val init = line(Rpc.Request(num(1), "initialize",
      obj("protocolVersion" -> Str("2025-06-18"), "capabilities" -> Json.Obj.empty,
        "clientInfo" -> obj("name" -> Str("t")))))

  def talk(lines: String*): List[Rpc] = {
    val server = face.server[String, L]("greeting", steps[String, F, L](greeting))
    Machine.run(servers.links.scripted(lines.toList)(servers.serve[L](server)))._1.map(rpc)
  }

  test("the actions as tools with their schemas; a call moves the dialogue and list_changed follows; look and back") {
    val replies = talk(init,
      line(Rpc.Request(num(2), "tools/list", Json.Obj.empty)),
      line(Rpc.Request(num(3), "tools/call",
          obj("name" -> Str("submit"), "arguments" -> obj("name" -> Str("Ann"), "age" -> num(41))))),
      line(Rpc.Request(num(4), "tools/call", obj("name" -> Str("look")))),
      line(Rpc.Request(num(5), "tools/call", obj("name" -> Str("back")))),
      line(Rpc.Request(num(6), "resources/read", obj("uri" -> Str("face://view")))))
    replies(1) match {
      case Rpc.Answer(_, r: Json.Obj) =>
        val tools = r.get("tools").collect { case Json.Arr(ts) => ts.toList.collect { case o: Json.Obj => o } }.get
        // no `back` yet: nothing to go back to at the first step; it is listed once there is
        assertEquals(tools.map(_.get("name").get), List(Str("submit"), Str("look")))
        assert(Json.print(tools.head.get("inputSchema").get).contains(
            "\"age\":{\"type\":\"integer\",\"description\":\"in years\"}"))
      case other => fail(s"$other")
    }
    replies(2) match {
      case Rpc.Answer(_, r: Json.Obj) =>
        assert(Json.print(r).contains("Greet Ann, 41?"), Json.print(r))
        assert(Json.print(r).contains("\"actions\":[\"yes\",\"no\"]"))
      case other => fail(s"$other")
    }
    assertEquals(replies(3), Rpc.Notify("notifications/tools/list_changed", Json.Obj.empty))
    replies(4) match {
      case Rpc.Answer(_, r: Json.Obj) => assert(Json.print(r).contains("Greet Ann, 41?")); case other => fail(s"$other")
    }
    replies(5) match {
      case Rpc.Answer(_, r: Json.Obj) => assert(Json.print(r).contains("Who are you?")); case other => fail(s"$other")
    }
    assertEquals(replies(6),
      Rpc.Notify("notifications/tools/list_changed", Json.Obj.empty)) // back: the tools are submit again
    replies(7) match {
      case Rpc.Answer(_, r: Json.Obj) => assert(Json.print(r).contains("# Who are you?")); case other => fail(s"$other")
    }
  }
  test("a bad argument is an error result with the problems; the step is unchanged; the dialogue ends with done") {
    val replies = talk(init,
      line(Rpc.Request(num(2), "tools/call",
          obj("name" -> Str("submit"), "arguments" -> obj("name" -> Str("Bo"), "age" -> num(3))))),
      line(Rpc.Request(num(3), "tools/call",
          obj("name" -> Str("submit"), "arguments" -> obj("name" -> Str("Bo"), "age" -> num(30))))),
      line(Rpc.Request(num(4), "tools/call", obj("name" -> Str("yes")))),
      line(Rpc.Request(num(5), "tools/list", Json.Obj.empty)))
    replies(1) match {
      case Rpc.Answer(_, r: Json.Obj) =>
        assertEquals(r.get("isError"), Some(Json.Bool(true))); assert(Json.print(r).contains("age: of age"));
      case other => fail(s"$other")
    }
    replies(2) match {
      case Rpc.Answer(_, r: Json.Obj) => assert(Json.print(r).contains("Greet Bo, 30?")); case other => fail(s"$other")
    }
    val last = replies.last
    last match {
      case Rpc.Answer(_, r: Json.Obj) => assertEquals(r.get("tools").collect { case Json.Arr(ts) => ts.size }, Some(2));
      case other => fail(s"$other")
    } // look and back: over, and the confirmation was a step to return to
    assert(replies.exists {
      case Rpc.Answer(_, r: Json.Obj) =>
        Json.print(r).contains("Hello, Bo!") && Json.print(r).contains("done") && !Json.print(r).contains("greeted Bo")
      case _ => false
    })
  }
  test("Markdown and JSON of views") {
    val v = View.stack(View.heading("T"), View.table("a", "b")(Vector(View.text("1"), View.Masked("••"))),
      View.fields("k" -> View.text("v")), View.items(View.text("x")))
    assertEquals(Markdown.view(v), "# T\n\n| a | b |\n| --- | --- |\n| 1 | •• |\n\n- **k:** v\n\n- x")
    assertEquals(Json.print(okay.faces.ViewJson.encode(View.Masked("••"))), """{"type":"masked","hint":"••"}""")
  }
  test("a mark in the vocabulary is shown in the Markdown; one not in it is seen through") {
    val vocab = Vocabulary("price" -> "the amount due")
    val v = View.stack(View.marked("price", View.text("12 €")), View.marked("badge", View.text("new")),
      View.items(View.marked("price", View.text("3"))), View.marked("price", View.items(View.text("a"))))
    assertEquals(Markdown.view(v), "12 €\n\nnew\n\n- 3\n\n- a")
    assertEquals(Markdown.view(v, vocab), "**price:** 12 €\n\nnew\n\n- price: 3\n\n**price**\n\n- a")
  }
  test("a server with a vocabulary tells it in the instructions and serves it as a resource") {
    val vocab = Vocabulary("price" -> "the amount due")
    val server = face.server[String, L]("greeting", steps[String, F, L](greeting), instructions = Some("Greet."),
      settings = McpFace.Settings(vocabulary = vocab))
    val replies = Machine.run(servers.links.scripted(List(init,
          line(Rpc.Request(num(2), "resources/list", Json.Obj.empty)),
          line(Rpc.Request(num(3), "resources/read", obj("uri" -> Str("face://marks"))))))(servers.serve[L](server)))._1
    val texts = replies
    assert(texts.head.contains("Greet.\\n\\nMarks on views, and what they mean:\\n- price: the amount due"), texts.head)
    assert(texts(1).contains("face://marks"), texts(1))
    assert(texts(2).contains("""[{\"name\":\"price\",\"doc\":\"the amount due\"}]"""), texts(2))
    val plain = face.server[String, L]("greeting", steps[String, F, L](greeting))
    val none =
      Machine.run(servers.links.scripted(List(init, line(Rpc.Request(num(2), "resources/list", Json.Obj.empty))))(
          servers.serve[L](plain)))._1
    assert(!none(1).contains("face://marks"), none(1))
  }
}
