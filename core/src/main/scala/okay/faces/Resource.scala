package okay.faces

import okay.effects.json.{Descent, Json}
import okay.effects.shape.{Problem, Problems}

/**
 * A STEP AS THE REST FACE SENDS IT: its `data` — the view, the affordances, whether back is possible, whether it is
 * done, whether the step listens (an event on the server may move it) — and `links` with `self`. What a remote face,
 * a page that knows no application, reads: the Laminar one and the DOM one alike.
 */
final case class Resource(self: String, view: View, actions: List[Action[Json]], back: Boolean, done: Boolean,
  listening: Boolean)

object Resource {
  /** the resource in a response's body; `fallbackSelf` its URL when it names none */
  def decode(fallbackSelf: String, body: String): Either[String, Resource] = for {
    j <- Descent.parse(body).left.map(_.message)
    o <- j match { case o: Json.Obj => Right(o); case x => Left(s"a resource is an object, not ${Json.kind(x)}") }
    data <- o.get("data") match { case Some(d: Json.Obj) => Right(d); case _ => Left("no data") }
    view <- data.get("view").toRight("no view").flatMap(ViewJson.decode)
    affordances <- data.get("actions") match {
      case Some(Json.Arr(xs)) => xs.foldLeft[Either[String, Vector[Affordance]]](Right(Vector.empty))((acc, x) =>
          acc.flatMap(as => Affordance.decode(x).map(as :+ _)))
      case _ => Right(Vector.empty)
    }
  } yield {
    val self = (o.get("links") match {
      case Some(Json.Arr(ls)) => ls.collectFirst {
          case l: Json.Obj if l.get("rel").contains(Json.Str("self")) => l.get("href")
        }.flatten.collect { case Json.Str(h) => h }
      case _ => None
    }).getOrElse(fallbackSelf)
    Resource(self, view, affordances.map(_.action).toList, data.get("back").contains(Json.Bool(true)),
      data.get("done").contains(Json.Bool(true)), data.get("listening").contains(Json.Bool(true)))
  }

  /** a `Trouble`'s problems, else its title */
  def problems(body: String): Problems = Descent.parse(body) match {
    case Right(o: Json.Obj) => o.get("problems") match {
        case Some(Json.Arr(ps)) => ps.toList.flatMap {
            case p: Json.Obj => Some(Problem(p.get("path").collect { case Json.Arr(xs) =>
                    xs.toList.collect { case Json.Str(s) => s }
                  }.getOrElse(Nil), p.get("message").collect { case Json.Str(m) => m }.getOrElse(""))); case _ => None
          }
        case _ => List(Problem(Nil, o.get("title").collect { case Json.Str(t) => t }.getOrElse(body)))
      }
    case _ => List(Problem(Nil, body))
  }
}
