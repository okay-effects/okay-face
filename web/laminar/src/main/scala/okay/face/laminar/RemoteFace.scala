package okay.face.laminar

import com.raquo.laminar.api.L._
import okay.effects.json.{Descent, Json}
import okay.effects.shape.{Problem, Problems}
import okay.face.{Action, Affordance, Chosen, Role, Step, View, ViewJson}
import okay.http.{Request, Response}
import org.scalajs.dom
import scala.concurrent.{ExecutionContext, Future}

/**
 * THE REMOTE FACE: a dialogue that lives on a server behind the REST face, drawn in the browser by Laminar. The client
 * knows no application — it draws whatever view comes, draws a form from whatever schema comes, and sends a `Chosen`
 * back — so an application written once as a program runs on the server with a rich page in front of it and not a line
 * of its own in the browser. The wire is the REST face's: `POST base` opens a session, its data is the view, the
 * affordances, whether back is possible and whether it is done; `POST self/act/{name}` with the arguments is a choice,
 * `400` with problems leaves the step; `POST self/back`. A choice is checked against the schema before it travels.
 * The transport is a function, `Browser.fetch()` in a page, the server's own service in a test.
 */
class RemoteFace(transport: Request => Future[Response], poll: Int = 1000)(implicit ec: ExecutionContext) {
  import RemoteFace.Shown
  private val nothing = Shown("", View.Empty, Nil, back = false, done = false, Nil, busy = true)

  /** the element: a session opened at mount, the step shown as it comes */
  def element(base: String = "/sessions"): HtmlElement = {
    val state: Var[Shown] = Var(nothing)
    def failed(ps: Problems): Unit = state.update(_.copy(problems = ps, busy = false))
    def arrived(self: String)(r: Response): Unit = r.status match {
      case 200 | 201 => RemoteFace.shown(r.headers.get("Location").orElse(r.headers.get("location")).getOrElse(self), r.body) match {
        case Right(s) => state.set(s)
        case Left(e) => failed(List(Problem(Nil, e)))
      }
      case 400 => failed(RemoteFace.problems(r.body))
      case other => failed(List(Problem(Nil, s"$other from the server")))
    }
    def call(req: Request, self: String): Unit = { state.update(_.copy(busy = true)); transport(req).map(arrived(self)).recover { case e => failed(List(Problem(Nil, e.getMessage))) } }
    def choose(s: Shown, chosen: Chosen): Unit = if (!s.busy) Step.take(chosen, s.actions) match {
      case Left(rejected) => failed(rejected.problems)
      case Right(args) => call(Request.json("POST", s"${s.self}/act/${chosen.action}", args), s.self)
    }
    def back(s: Shown): Unit = if (!s.busy && s.back) call(Request("POST", s"${s.self}/back"), s.self)
    // a step that listens on the server: asked again every `poll` ms, until it moves on
    def again(s: Shown): Unit = if (s.listening && !s.busy) dom.window.setTimeout(() => { val now = state.now(); if ((now eq s) && now.listening && !now.busy) call(Request("GET", now.self), now.self) }, poll.toDouble)

    div(cls := Role.Face.classes,
      onMountCallback(_ => call(Request("POST", base), base)),
      child <-- state.signal.map { s =>
        again(s)
        val problems = if (s.problems.isEmpty) emptyNode else ul(cls := Role.Problems.classes, s.problems.map(p => li(p.render)))
        val backButton = if (!s.back) emptyNode else button(typ := "button", cls := Role.Back.classes, "Back", disabled := s.busy, onClick --> Observer[dom.MouseEvent](_ => back(s)))
        val ref: Option[String => Unit] = if (s.actions.exists(_.name == "ref")) Some(key => choose(s, Chosen("ref", Json.obj("key" -> Json.Str(key))))) else None
        div(sectionTag(cls := Role.View.classes, Views.element(s.view, ref)), problems,
          if (s.done) div(cls := "okay-done", "done") else navTag(cls := Role.Actions.classes, s.actions.map(a => Forms.action(a, choose(s, _), s.busy)), backButton))
      })
  }
}

object RemoteFace {
  def apply(transport: Request => Future[Response], poll: Int = 1000)(implicit ec: ExecutionContext): RemoteFace = new RemoteFace(transport, poll)
  /** what the page shows: the session's URL, the step as it came, the problems of the last choice, whether a call is out */
  final case class Shown(self: String, view: View, actions: List[Action[Json]], back: Boolean, done: Boolean, problems: Problems, busy: Boolean, listening: Boolean = false)

  /** the REST face's resource: `data` with the view, the affordances, back, done; `links` with self */
  def shown(fallbackSelf: String, body: String): Either[String, Shown] = for {
    j <- Descent.parse(body).left.map(_.message)
    o <- j match { case o: Json.Obj => Right(o); case x => Left(s"a resource is an object, not ${Json.kind(x)}") }
    data <- o.get("data") match { case Some(d: Json.Obj) => Right(d); case _ => Left("no data") }
    view <- data.get("view").toRight("no view").flatMap(ViewJson.decode)
    affordances <- data.get("actions") match {
      case Some(Json.Arr(xs)) => xs.foldLeft[Either[String, Vector[Affordance]]](Right(Vector.empty))((acc, x) => acc.flatMap(as => Affordance.decode(x).map(as :+ _)))
      case _ => Right(Vector.empty)
    }
  } yield {
    val self = (o.get("links") match {
      case Some(Json.Arr(ls)) => ls.collectFirst { case l: Json.Obj if l.get("rel").contains(Json.Str("self")) => l.get("href") }.flatten.collect { case Json.Str(h) => h }
      case _ => None
    }).getOrElse(fallbackSelf)
    val back = data.get("back").contains(Json.Bool(true))
    val done = data.get("done").contains(Json.Bool(true))
    val listening = data.get("listening").contains(Json.Bool(true))
    Shown(self, view, affordances.map(_.action).toList, back, done, Nil, busy = false, listening)
  }
  /** a `Trouble`'s problems, else its title */
  def problems(body: String): Problems = Descent.parse(body) match {
    case Right(o: Json.Obj) => o.get("problems") match {
      case Some(Json.Arr(ps)) => ps.toList.flatMap { case p: Json.Obj => Some(Problem(p.get("path").collect { case Json.Arr(xs) => xs.toList.collect { case Json.Str(s) => s } }.getOrElse(Nil), p.get("message").collect { case Json.Str(m) => m }.getOrElse(""))); case _ => None }
      case _ => List(Problem(Nil, o.get("title").collect { case Json.Str(t) => t }.getOrElse(body)))
    }
    case _ => List(Problem(Nil, body))
  }
}
