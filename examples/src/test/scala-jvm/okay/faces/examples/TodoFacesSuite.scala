package okay.faces.examples

import javax.swing.{JButton, JTextField}
import okay.effects.json.Json
import okay.effects.json.Json.{Str, num, obj}
import okay.effects.machine.Machine._
import okay.faces.examples.Todo._
import okay.faces.html.HtmlFace
import okay.faces.mcp.McpFace
import okay.faces.rest.RestFace
import okay.faces.swing.SwingFace
import okay.faces.term.Terminal
import okay.faces.{Chosen, View}
import okay.http.{Http, Https, Request}
import okay.mcp.{Link, Links, Rpc}

/** THE FACES HANDLE A PROGRAM THEY WERE NOT WRITTEN FOR: the list, whole, on each — the form asked by prompts, by a
 * dialog's fields, by a page's inputs, by a tool's arguments; the items clicked; the same list at the end */
class TodoFacesSuite extends munit.FunSuite {
  System.setProperty("java.awt.headless", "true")
  type H[X] = Row[*[Pure] + *[Http], X]
  type L[X] = Row[*[Pure] + *[Link], X]
  val https = new Https()
  val links = new Links()
  def all(c: java.awt.Component): List[java.awt.Component] = c match {
    case p: java.awt.Container => c :: p.getComponents.toList.flatMap(all)
    case _ => List(c)
  }

  test("the terminal: the form field by field, an optional one asked for, an item by its number; the list at the end") {
    val term = Terminal()
    val io = new okay.effects.world.Io()
    // add: title, due? no, note? no; add: title, due? yes and the day, note? no; item 1 by its key; leave
    val lines = List("add", "milk", "n", "n", "add", "bread", "y", "2026-10-25", "n", "1", "leave")
    val (printed, ended) = run(io.console(lines)(term.handle(Todo.keeping)))
    val t = ended.getOrElse(fail(s"not ended:\n${printed.mkString("\n")}"))
    assertEquals(t.items.map(_._2.title), Vector("milk", "bread")); assertEquals(t.done, Set(1))
    assert(printed.exists(_.contains("added: milk")) && printed.exists(_.contains("due 2026-10-25")),
      printed.mkString("\n"))
    assert(printed.exists(_.contains("1 of 2 done")), printed.mkString("\n"))
  }

  test("the Swing panel: the form's fields filled and submitted, an item a button, the list at the end") {
    val face = SwingFace()
    var result = Option.empty[Todos]
    val root = face.panel[Todos](face.faces.steps[Todos, okay.faces.Face, Pure](face.faces.of(Todo.keeping)),
      (t: Todos) => result = Some(t))
    def byName[C](name: String): C = all(root).collectFirst { case c if c.getName == name => c.asInstanceOf[C] }
      .getOrElse(fail(s"no $name among ${all(root).map(_.getName).filter(_ != null).mkString(", ")}"))
    byName[JTextField]("f-title").setText("milk"); byName[JButton]("submit:add").doClick()
    byName[JTextField]("f-title").setText("bread"); byName[JButton]("submit:add").doClick()
    all(root).collectFirst { case b: JButton if b.getToolTipText == "1" => b }.getOrElse(fail("no item 1")).doClick()
    byName[JButton]("action:leave").doClick()
    assertEquals(result.map(_.items.map(_._2.title)), Some(Vector("milk", "bread")));
    assertEquals(result.map(_.done), Some(Set(1)))
  }

  test("the HTML pages: the form posted, an item a link, the list ended") {
    val html = HtmlFace()
    val pages = html.handle(Todo.keeping)
    val form = "application/x-www-form-urlencoded"
    val requests = List(Request.get("/"), Request.get("/s/s1"), Request.post("/s/s1/act/add", "title=milk", form),
      Request.post("/s/s1/act/add", "title=bread&due=2026-10-25", form), Request.get("/s/s1"),
      Request.get("/s/s1/ref/1"), Request.post("/s/s1/act/leave", "", form), Request.get("/s/s1"))
    val responses = run(https.scripted(requests)(https.serve[H](pages)))._1
    assertEquals(responses.map(_.status), List(303, 200, 303, 303, 200, 303, 303, 200))
    val listed = responses(4).body
    assert(listed.contains("""href="/s/s1/ref/2"""") && listed.contains("okay-mark-due") && listed.contains("f-title"),
      listed)
    val last = responses.last.body
    assert(last.contains("done: Todos(") && last.contains("milk") && last.contains("bread"), last)
  }

  test("the REST resources: the form as a tool with its schema, an item by key, the list ended") {
    val rest = RestFace()
    val sessions = rest.handle(Todo.keeping, vocabulary = marks.vocabulary)
    val requests = List(Request.post("/sessions", "", "application/json"),
      Request.json("POST", "/sessions/s1/act/add", obj("title" -> Str("milk"))),
      Request.json("POST", "/sessions/s1/act/ref", obj("key" -> Str("1"))),
      Request.json("POST", "/sessions/s1/act/add", obj()),
      Request.json("POST", "/sessions/s1/act/leave", obj()))
    val responses = run(https.scripted(requests)(https.serve[H](sessions)))._1
    assertEquals(responses.map(_.status), List(201, 200, 200, 400, 200))
    assert(responses.head.body.contains("\"name\":\"add\"") && responses.head.body.contains("\"title\""),
      responses.head.body)
    assert(responses(2).body.contains("okay-mark-done") || responses(2).body.contains("\"mark\":\"done\""),
      responses(2).body)
    assert(responses(3).body.contains("title"), responses(3).body)
    assert(responses.last.body.contains("\"done\":true"), responses.last.body)
  }

  test("MCP: the form a tool with the item's schema; a call adds, the items are the view's refs") {
    val mcp = McpFace()
    val served = mcp.handle(Todo.keeping, vocabulary = marks.vocabulary)
    val init = Rpc.encode(Rpc.Request(num(1), "initialize",
        obj("protocolVersion" -> Str("2025-06-18"), "capabilities" -> Json.Obj.empty,
          "clientInfo" -> obj("name" -> Str("t")))))
    val lines = List(init, Rpc.encode(Rpc.Request(num(2), "tools/list", Json.Obj.empty)),
      Rpc.encode(Rpc.Request(num(3), "tools/call",
          obj("name" -> Str("add"), "arguments" -> obj("title" -> Str("milk"))))),
      Rpc.encode(Rpc.Request(num(4), "resources/read", obj("uri" -> Str("face://view")))))
    val (sent, _) = run(links.scripted(lines)(served))
    assert(sent.head.contains("\"name\":\"To do\"") && sent.head.contains("Your list"), sent.head)
    assert(sent(1).contains("\"name\":\"add\"") && sent(1).contains("\"title\"") && sent(1).contains("\"due\""),
      sent(1))
    // the add changes the tools (an item can be done now), so a list_changed is sent too: look by content
    assert(sent.exists(_.contains("added: milk")), sent.mkString("\n"))
    assert(sent.exists(_.contains("[milk](#1)")), sent.mkString("\n"))
  }
}
