package okay.face.examples

import javax.swing.JButton
import okay.effects.{Sum, Void}
import okay.effects.json.Json
import okay.effects.json.Json.{Str, obj}
import okay.effects.machine.Machine
import okay.effects.world.Console
import okay.face.{Chosen, Face, Faces, View, ViewJson}
import okay.face.html.{Html, HtmlFace}
import okay.face.rest.RestFace
import okay.face.swing.SwingFace
import okay.face.term.{Render, Terminal}
import okay.face.examples.Noughts._
import okay.face.test.Plays
import okay.http.{Http, Https, Request}

/** THE FACES AGREE: one game, one script of choices — X down the left, the program answering — and every face shows
 * the test face's views, in order, and ends the same. The test face is the reference: what it shows is what the
 * program said; a face that shows more, less or otherwise is wrong */
class AgreementSuite extends munit.FunSuite {
  System.setProperty("java.awt.headless", "true")
  val faces: Faces[Machine.type] = new Faces[Machine.type]()(Machine)
  val plays: Plays[Machine.type] = new Plays[Machine.type]()(Machine)
  val game = new Noughts[Machine.type]()(faces)
  import game._
  import Machine._
  type F[X] = Sum[Void, Face, X]
  val moves = List(1, 4, 7)
  def cell(n: Int): Chosen = Chosen("ref", obj("key" -> Str(n.toString)))

  /** the reference: the views the program shows, and how it ends */
  val (views, outcome) = plays.play(faces.steps[Outcome, F, Void](game.game[F](Board.empty)), plays.Script.of(moves.map(cell) :+ Chosen.plain("again"): _*)) match {
    case plays.Played.Finished(vs, o) => (vs, o)
    case other => fail(s"the reference did not finish: $other")
  }
  test("the reference: four views — three boards offered, the end told — and X won") {
    assertEquals(views.size, 4); assertEquals(outcome, Won('X'))
    assert(views.last match { case View.Group(_, children) => children.contains(game.marks.mark(game.Result, View.text("X won"))); case _ => false }, views.last.toString)
  }
  test("the game's typed mark: named from its case, carried by HTML as a class, by REST as JSON, by Swing as a name, read back as the type") {
    val line = game.marks.mark(game.Result, View.text("X won"))
    assertEquals(game.marks.name(game.Result), "result"); assertEquals(game.marks.on(line), Some(game.Result))
    assert(Html.view(line).contains("""<span class="okay-mark okay-mark-result"><p class="okay-text">X won</p></span>"""), Html.view(line))
    assertEquals(ViewJson.encode(line), obj("type" -> Str("marked"), "mark" -> Str("result"), "view" -> obj("type" -> Str("text"), "text" -> Str("X won"))))
    assertEquals(okay.face.swing.Views.component(line).getName, "okay-mark-result")
    assertEquals(game.marks.vocabulary.markdown, "- result: how the game ended: who won, or a draw\n- cell: a cell of the board: X, O, or free to play")
    val css = okay.face.Css(okay.face.Look(okay.face.Theme.light, okay.face.Style.default.adjust(game.marks.role(game.Result))(_.copy(bold = true))))
    assert(css.contains(".okay-mark-result{font-weight:bold}"), css)
  }

  test("the terminal shows every view as the test face's, rendered, in order, and ends the same") {
    type C[X] = Sum[Void, Console, X]
    type CF[X] = Sum[C, Face, X]
    val term = new Terminal[Machine.type]()(faces)
    val inputs = moves.map(_.toString) :+ "again"
    val (printed, ended) = Machine.run(term.io.console(inputs)(term.terminal[Outcome, C](faces.steps[Outcome, CF, C](game.game[CF](Board.empty)))))
    assertEquals(ended, Some(outcome))
    val text = printed.mkString("\n")
    val positions = views.map(v => text.indexOf(Render.view(v).mkString("\n")))
    assert(positions.forall(_ >= 0), s"a view not printed: $positions\n$text")
    assertEquals(positions, positions.sorted, "in order")
  }

  test("the Swing panel shows every view's text, and ends the same") {
    val face = new SwingFace[Machine.type]()(faces)
    var result = Option.empty[Outcome]
    val root = face.panel[Outcome](faces.steps[Outcome, F, Void](game.game[F](Board.empty)), (o: Outcome) => result = Some(o))
    def all(c: java.awt.Component): List[java.awt.Component] = c match { case p: java.awt.Container => c :: p.getComponents.toList.flatMap(all); case _ => List(c) }
    def byName[C <: java.awt.Component](name: String): C = all(root).find(_.getName == name).getOrElse(fail(s"no $name")).asInstanceOf[C]
    def shown: String = all(root).collect { case l: javax.swing.JLabel => l.getText; case t: javax.swing.table.JTableHeader => ""; case a: javax.swing.JTextArea => a.getText }.mkString("\n")
    val seen = scala.collection.mutable.ListBuffer(shown)
    // a free cell is a button, told apart by its tooltip, the key
    moves.foreach { n => all(root).collectFirst { case b: JButton if b.getToolTipText == n.toString => b }.getOrElse(fail(s"no cell $n")).doClick(); seen += shown }
    byName[JButton]("action:again").doClick(); seen += shown
    assertEquals(result, Some(outcome))
    views.zip(seen.toList).foreach { case (v, s) => assert(s.contains(okay.face.swing.Views.text(View.Heading(1, "Noughts and crosses"))) || s.contains("won"), s) }
    assert(seen.init.last.contains("X won") && seen.last.contains("done: Won(X)"), seen.takeRight(2).mkString("\n---\n"))
  }

  test("the HTML pages show every view as the test face's, marked up, in order, and end the same") {
    type H[X] = Sum[Void, Http, X]
    type HF[X] = Sum[H, Face, X]
    val https: Https[Machine.type] = new Https[Machine.type]()(Machine)
    val pages = HtmlFace(faces).pages[Outcome, H](() => faces.dialogue[Outcome, H](faces.steps[Outcome, HF, H](game.game[HF](Board.empty))), "Noughts")
    val requests = Request.get("/") :: Request.get("/s/s1") :: (moves.flatMap(n => List(
      Request.get(s"/s/s1/ref/$n"), Request.get("/s/s1"))) ++
      List(Request.post("/s/s1/act/again", "", "application/x-www-form-urlencoded"), Request.get("/s/s1")))
    val responses = Machine.run(https.scripted(requests)(https.serve[H](pages.service)))._1
    val gets = responses.filter(_.status == 200).map(_.body)
    assertEquals(gets.size, 5)
    // a waiting step's refs are links to play; the ended board's are names
    views.zip(gets).foreach { case (v, page) => assert(page.contains(Html.view(v, Some("/s/s1"))) || page.contains(Html.view(v)), s"the page does not show the view:\n${Html.view(v, Some("/s/s1"))}\n$page") }
    assert(gets(1).contains("""href="/s/s1/ref/2""""), "a free cell is a link to play it")
    val noBack = HtmlFace(faces).pages[Outcome, H](() => faces.dialogue[Outcome, H](faces.steps[Outcome, HF, H](game.game[HF](Board.empty)), back = false), "Noughts")
    val after = Machine.run(https.scripted(requests)(https.serve[H](noBack.service)))._1.filter(_.status == 200).map(_.body)
    assert(after.size == 5 && after.forall(!_.contains("okay-back")), "a dialogue without back offers none")
    assert(gets.last.contains("done: Won(X)"))
  }

  test("the REST resources carry every view as the test face's, as JSON, in order, and end the same") {
    type H[X] = Sum[Void, Http, X]
    type HF[X] = Sum[H, Face, X]
    val https: Https[Machine.type] = new Https[Machine.type]()(Machine)
    val sessions = RestFace(faces).sessions[Outcome, H](() => faces.dialogue[Outcome, H](faces.steps[Outcome, HF, H](game.game[HF](Board.empty))))
    val requests = Request.post("/sessions", "", "application/json") :: (moves.map(n => Request.json("POST", "/sessions/s1/act/ref", obj("key" -> Str(n.toString)))) :+ Request.json("POST", "/sessions/s1/act/again", obj()))
    val responses = Machine.run(https.scripted(requests)(https.serve[H](sessions.service)))._1
    assertEquals(responses.map(_.status), List(201, 200, 200, 200, 200))
    // a resource's view is what was told since the last step and the step's own view; at the end, the told and "done" once "again" is taken
    val shown = responses.map(r => okay.effects.json.Descent.parse(r.body).toOption.flatMap {
      case o: Json.Obj => o.get("data").collect { case d: Json.Obj => d }.flatMap(_.get("view")).collect { case v: Json.Obj => v.get("children") }.flatten.collect { case Json.Arr(cs) => cs.toList }
      case _ => None
    }.getOrElse(fail(s"no view in ${r.body}")))
    views.zip(shown).foreach { case (v, cs) => assert(cs.contains(ViewJson.encode(v)), s"not carried: $v in $cs") }
    assertEquals(shown.init.map(_.last), views.map(ViewJson.encode), "a waiting step's own view is the last child; the end is offered too")
    assert(responses.last.body.contains("\"done\":true"))
  }
}
