package okay.faces.examples

import javax.swing.JButton
import okay.effects.json.Json
import okay.effects.json.Json.{Str, obj}
import okay.effects.machine.Machine._
import okay.faces.{Chosen, View, ViewJson}
import okay.faces.html.{Html, HtmlFace}
import okay.faces.rest.RestFace
import okay.faces.swing.SwingFace
import okay.faces.term.{Render, Terminal}
import okay.faces.examples.Noughts._
import okay.faces.test.Plays
import okay.http.{Http, Https, Request}

/** THE FACES AGREE: one program, one script of choices — X down the left, the program answering — and every face shows
 * the test face's views, in order, and ends the same. The test face is the reference: what it shows is what the
 * program said; a face that shows more, less or otherwise is wrong. Every face is a handler of the one effect, given
 * the one program, nothing else */
class AgreementSuite extends munit.FunSuite {
  System.setProperty("java.awt.headless", "true")
  val plays = Plays()
  type H[X] = Row[*[Pure] + *[Http], X]
  val https = new Https()
  val moves = List(1, 4, 7)
  def cell(n: Int): Chosen = Chosen("ref", obj("key" -> Str(n.toString)))

  /** the reference: the views the program shows, and how it ends */
  val (views, outcome) =
    plays.handle(Noughts.play(Board.empty))(plays.Script.of(moves.map(cell) :+ Chosen.plain("again"): _*)) match {
      case plays.Played.Finished(vs, o) => (vs, o)
      case other => fail(s"the reference did not finish: $other")
    }
  test("the reference: four views — three boards offered, the end — and X won") {
    assertEquals(views.size, 4); assertEquals(outcome, Won(X))
    assert(views.last match {
        case View.Group(_, children) => children.contains(marks.mark(Result, View.text("X won"))); case _ => false
      }, views.last.toString)
  }
  test(
    "the game's typed mark: named from its case, carried by HTML as a class, by REST as JSON, by Swing as a name, " +
      "read back as the type") {
    val line = marks.mark(Result, View.text("X won"))
    assertEquals(marks.name(Result), "result"); assertEquals(marks.on(line), Some(Result))
    assert(Html.view(line).contains(
        """<span class="okay-mark okay-mark-result"><p class="okay-text">X won</p></span>"""), Html.view(line))
    assertEquals(ViewJson.encode(line),
      obj("type" -> Str("marked"), "mark" -> Str("result"),
        "view" -> obj("type" -> Str("text"), "text" -> Str("X won"))))
    assertEquals(okay.faces.swing.Views.component(line).getName, "okay-mark-result")
    assertEquals(marks.vocabulary.markdown,
      "- result: how the game ended: who won, or a draw\n- cell: a cell of the board: X, O, or free to play")
    assert(okay.faces.Css(look).contains(".okay-mark-result{"), okay.faces.Css(look))
  }

  test("the terminal shows every view as the test face's, rendered, in order, and ends the same") {
    val term = Terminal()
    val io = new okay.effects.world.Io()
    val (printed, ended) = run(io.console(moves.map(_.toString) :+ "again")(term.handle(Noughts.play(Board.empty))))
    assertEquals(ended, Some(outcome))
    val text = printed.mkString("\n")
    val positions = views.map(v => text.indexOf(Render.view(v).mkString("\n")))
    assert(positions.forall(_ >= 0), s"a view not printed: $positions\n$text")
    assertEquals(positions, positions.sorted, "in order")
  }

  test("the Swing panel shows every view's text, and ends the same") {
    val face = SwingFace()
    var result = Option.empty[Outcome]
    val root = face.panel[Outcome](face.faces.steps[Outcome, okay.faces.Face, Pure](
        face.faces.of(Noughts.play(Board.empty))), (o: Outcome) => result = Some(o))
    def all(c: java.awt.Component): List[java.awt.Component] =
      c match { case p: java.awt.Container => c :: p.getComponents.toList.flatMap(all); case _ => List(c) }
    def shown: String = all(root).collect {
      case l: javax.swing.JLabel => l.getText; case t: javax.swing.table.JTableHeader => "";
      case a: javax.swing.JTextArea => a.getText
    }.mkString("\n")
    val seen = scala.collection.mutable.ListBuffer(shown)
    // a free cell is a button, told apart by its tooltip, the key
    moves.foreach { n =>
      all(root).collectFirst { case b: JButton if b.getToolTipText == n.toString => b }.getOrElse(
        fail(s"no cell $n")).doClick(); seen += shown
    }
    all(root).collectFirst { case b: JButton if b.getName == "action:again" => b }.getOrElse(
      fail("no again")).doClick(); seen += shown
    assertEquals(result, Some(outcome))
    assert(seen.init.last.contains("X won") && seen.last.contains("done"),
      seen.takeRight(2).mkString("\n---\n"))
    assert(!all(root).exists(_.getName == "okay-back"), "no back: the step refuses it")
  }

  test("the HTML pages show every view as the test face's, marked up, in order, and end the same") {
    val html = HtmlFace() // bound first: Scala 3 widens a word on an unbound face to Any
    val pages =
      html.handle(Noughts.play(Board.empty), okay.faces.html.HtmlFace.Settings(ids = okay.faces.Sessions.counting("s")))
    val requests = Request.get("/") :: Request.get("/s/s1") :: (moves.flatMap(n =>
      List(Request.get(s"/s/s1/ref/$n"), Request.get("/s/s1"))) ++
      List(Request.post("/s/s1/act/again", "", "application/x-www-form-urlencoded"), Request.get("/s/s1")))
    val responses = run(https.scripted(requests)(https.serve[H](pages)))._1
    val gets = responses.filter(_.status == 200).map(_.body)
    assertEquals(gets.size, 5)
    // a waiting step's refs are links to play; the ended board's are names
    views.zip(gets).foreach { case (v, page) =>
      assert(page.contains(Html.view(v, Some("/s/s1"))) || page.contains(Html.view(v)),
        s"the page does not show the view:\n${Html.view(v, Some("/s/s1"))}\n$page")
    }
    assert(gets.head.contains("""href="/s/s1/ref/2"""") && gets.head.contains("<title>Noughts and crosses</title>"),
      "a free cell is a link to play it; the page titled by the heading")
    assert(gets.forall(!_.contains("okay-back")), "no back: the steps refuse it")
    assert(gets.last.contains("okay-done") || gets.last.contains("okay-notice-info"))
  }

  test("the REST resources carry every view as the test face's, as JSON, in order, and end the same") {
    val rest = RestFace()
    val sessions =
      rest.handle(Noughts.play(Board.empty), okay.faces.rest.RestFace.Settings(ids = okay.faces.Sessions.counting("s")))
    val requests = Request.post("/sessions", "", "application/json") :: (moves.map(n =>
      Request.json("POST", "/sessions/s1/act/ref", obj("key" -> Str(n.toString)))) :+ Request.json("POST",
      "/sessions/s1/act/again", obj()))
    val responses = run(https.scripted(requests)(https.serve[H](sessions)))._1
    assertEquals(responses.map(_.status), List(201, 200, 200, 200, 200))
    // a resource's view is what was told since the last step and the step's own view; at the end, "done" once "again"
    // is taken
    val shown = responses.map(r =>
      okay.effects.json.Descent.parse(r.body).toOption.flatMap {
        case o: Json.Obj => o.get("data").collect { case d: Json.Obj => d }.flatMap(_.get("view")).collect {
            case v: Json.Obj => v.get("children")
          }.flatten.collect { case Json.Arr(cs) => cs.toList }
        case _ => None
      }.getOrElse(fail(s"no view in ${r.body}")))
    views.zip(shown).foreach { case (v, cs) => assert(cs.contains(ViewJson.encode(v)), s"not carried: $v in $cs") }
    assertEquals(shown.init.map(_.last), views.map(ViewJson.encode),
      "a waiting step's own view is the last child; the end is offered too")
    assert(responses.forall(_.body.contains("\"back\":false")), "no back")
    assert(responses.last.body.contains("\"done\":true"))
  }
}
