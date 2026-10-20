package okay.face

import okay.effects.json.Json
import okay.effects.shape.Problems

/**
 * THE EFFECT: a representation offered with the actions available, answered by the action taken. A console program's
 * `println` and `readLine`, Delphi's `ShowModal`, an MCP server's elicitation: the program leads, the counterpart
 * answers, and the answer is the way up — a click, a tool call, a line typed, each the handler's business. A long-lived
 * screen is a loop of offers.
 */
sealed trait Face[X]
object Face {
  /** show, and wait for one of the actions, taken with its parameters */
  final case class Offer[A, X](view: View, actions: List[Action[A]], answer: A => X) extends Face[X]
  /** show, and go on */
  final case class Show[X](view: View, answer: Unit => X) extends Face[X]
}

/** what a face sends back: an action's name and its arguments, a JSON object */
final case class Chosen(action: String, args: Json = Json.Obj.empty)
object Chosen {
  def plain(action: String): Chosen = Chosen(action, Json.Obj.empty)
}

/** a choice no step's action fits, or arguments that did not decode: the face shows the problems and waits on the same step */
final case class Rejected(problems: Problems)
