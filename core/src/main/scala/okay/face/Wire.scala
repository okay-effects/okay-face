package okay.face

import okay.effects.json.Json
import okay.effects.shape.{Schema, Shape}

/**
 * WHAT CROSSES A WIRE between a dialogue and a face elsewhere: the view as JSON, and an action as an AFFORDANCE — its
 * name, label and doc with its parameters' erased `Schema` instead of the `Shape`. The REST face sends these; a client
 * that knows no application (the remote Laminar face) draws the view, draws a form from the schema, and sends back a
 * `Chosen`. `Affordance.action` is the client's `Action`: structural over the schema, so a choice is checked where it is
 * made before it travels.
 */
final case class Affordance(name: String, label: String, doc: Option[String], params: Schema) {
  def json: Json = Json.Obj(Vector("name" -> Json.Str(name), "label" -> Json.Str(label)) ++ doc.map(d => "doc" -> Json.Str(d)) :+ ("params" -> Schema.shape.encode(params)))
  /** the action a client offers: the arguments checked against the schema, kept as JSON */
  def action: Action[Json] = Action[Json, Json](name, label, Schema.structural(params), doc)(j => j)
}
object Affordance {
  def decode(j: Json): Either[String, Affordance] = j match {
    case o: Json.Obj => for {
      name <- str(o, "name"); label <- str(o, "label")
      params <- o.get("params").toRight("no params").flatMap(p => Schema.shape.decode(p).left.map(okay.effects.shape.Problem.render))
    } yield Affordance(name, label, o.get("doc").collect { case Json.Str(d) => d }, params)
    case other => Left(s"an affordance is an object, not ${Json.kind(other)}")
  }
  private def str(o: Json.Obj, k: String): Either[String, String] = o.get(k) match { case Some(Json.Str(s)) => Right(s); case _ => Left(s"no $k") }
}

/** a `View` as JSON, `{"type": ..., ...}`, and back */
object ViewJson {
  def encode(v: View): Json = v match {
    case View.Empty => Json.obj("type" -> Json.Str("empty"))
    case View.Text(s) => Json.obj("type" -> Json.Str("text"), "text" -> Json.Str(s))
    case View.Heading(level, s) => Json.obj("type" -> Json.Str("heading"), "level" -> Json.num(level), "text" -> Json.Str(s))
    case View.Notice(level, s) => Json.obj("type" -> Json.Str("notice"), "level" -> Json.Str(level.toString.toLowerCase), "text" -> Json.Str(s))
    case View.Fields(rows) => Json.obj("type" -> Json.Str("fields"), "rows" -> Json.Arr(rows.map { case (k, x) => Json.obj("label" -> Json.Str(k), "value" -> encode(x)) }))
    case View.Table(columns, rows) => Json.obj("type" -> Json.Str("table"), "columns" -> Json.Arr(columns.map(Json.Str(_))), "rows" -> Json.Arr(rows.map(r => Json.Arr(r.map(encode)))))
    case View.Items(items) => Json.obj("type" -> Json.Str("items"), "items" -> Json.Arr(items.map(encode)))
    case View.Group(title, children) => Json.Obj(Vector("type" -> Json.Str("group")) ++ title.map(t => "title" -> Json.Str(t)) :+ ("children" -> Json.Arr(children.map(encode))))
    case View.Ref(key, label) => Json.obj("type" -> Json.Str("ref"), "key" -> Json.Str(key), "label" -> Json.Str(label))
    case View.Datum(json, schema) => Json.Obj(Vector("type" -> Json.Str("datum"), "value" -> json) ++ schema.map(s => "schema" -> Schema.shape.encode(s)))
    case View.Masked(hint) => Json.obj("type" -> Json.Str("masked"), "hint" -> Json.Str(hint))
  }

  def decode(j: Json): Either[String, View] = j match {
    case o: Json.Obj => o.get("type") match {
      case Some(Json.Str("empty")) => Right(View.Empty)
      case Some(Json.Str("text")) => str(o, "text").map(View.Text(_))
      case Some(Json.Str("heading")) => for { l <- int(o, "level"); s <- str(o, "text") } yield View.Heading(l, s)
      case Some(Json.Str("notice")) => for {
        l <- str(o, "level").flatMap { case "info" => Right(View.Notice.Info); case "warning" => Right(View.Notice.Warning); case "problem" => Right(View.Notice.Problem); case x => Left(s"no notice level $x") }
        s <- str(o, "text") } yield View.Notice(l, s)
      case Some(Json.Str("fields")) => arr(o, "rows").flatMap(rows => all(rows.map {
        case r: Json.Obj => for { k <- str(r, "label"); v <- r.get("value").toRight("no value").flatMap(decode) } yield (k, v)
        case x => Left(s"a row is an object, not ${Json.kind(x)}")
      })).map(View.Fields(_))
      case Some(Json.Str("table")) => for {
        cs <- arr(o, "columns").flatMap(cs => all(cs.map { case Json.Str(c) => Right(c); case x => Left(s"a column is text, not ${Json.kind(x)}") }))
        rs <- arr(o, "rows").flatMap(rs => all(rs.map { case Json.Arr(cells) => all(cells.map(decode)); case x => Left(s"a row is an array, not ${Json.kind(x)}") }))
      } yield View.Table(cs, rs)
      case Some(Json.Str("items")) => arr(o, "items").flatMap(xs => all(xs.map(decode))).map(View.Items(_))
      case Some(Json.Str("group")) => arr(o, "children").flatMap(xs => all(xs.map(decode))).map(View.Group(o.get("title").collect { case Json.Str(t) => t }, _))
      case Some(Json.Str("ref")) => for { k <- str(o, "key"); l <- str(o, "label") } yield View.Ref(k, l)
      case Some(Json.Str("datum")) => o.get("value").toRight("no value").flatMap(v => o.get("schema") match {
        case None => Right(View.Datum(v, None))
        case Some(s) => Schema.shape.decode(s).left.map(okay.effects.shape.Problem.render).map(sc => View.Datum(v, Some(sc)))
      })
      case Some(Json.Str("masked")) => str(o, "hint").map(View.Masked(_))
      case Some(Json.Str(t)) => Left(s"no view type $t")
      case _ => Left("a view has a type")
    }
    case other => Left(s"a view is an object, not ${Json.kind(other)}")
  }
  private def str(o: Json.Obj, k: String): Either[String, String] = o.get(k) match { case Some(Json.Str(s)) => Right(s); case _ => Left(s"no $k") }
  private def int(o: Json.Obj, k: String): Either[String, Int] = o.get(k) match { case Some(Json.Num(n)) => Right(n.toInt); case _ => Left(s"no $k") }
  private def arr(o: Json.Obj, k: String): Either[String, Vector[Json]] = o.get(k) match { case Some(Json.Arr(xs)) => Right(xs); case _ => Left(s"no $k") }
  private def all[A](xs: Vector[Either[String, A]]): Either[String, Vector[A]] =
    xs.foldLeft[Either[String, Vector[A]]](Right(Vector.empty))((acc, x) => acc.flatMap(as => x.map(as :+ _)))
}
