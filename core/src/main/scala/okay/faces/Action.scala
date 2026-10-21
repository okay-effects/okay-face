package okay.faces

import okay.effects.shape.Shape

/**
 * ONE AFFORDANCE: what can be done now, with what. The list of a step's actions is Delphi's enabled buttons, HATEOAS's
 * links, an MCP tool list; an action's `Shape` is its form for a person, its `inputSchema` for an agent, its flags for
 * a command line. `name` is the wire's: `[a-z][a-z0-9_-]*`, unique within one offer.
 */
sealed abstract class Action[A] {
  type P
  def name: String
  def label: String
  def doc: Option[String]
  def params: Shape[P]
  def make: P => A
  /** the action taken with these arguments, decoded by its shape */
  final def taken(args: okay.effects.json.Json): Either[okay.effects.shape.Problems, A] = params.decode(args).map(make)
  /** the action as it crosses a wire: the shape erased to its schema */
  final def affordance: Affordance = Affordance(name, label, doc, params.erased)
}

object Action {
  def apply[P0, A](name0: String, label0: String, params0: Shape[P0], doc0: Option[String] = None)(make0: P0 => A): Action[A] = {
    require(name0.matches("[a-z][a-z0-9_-]*"), s"an action's name is [a-z][a-z0-9_-]*, not '$name0'")
    new Action[A] {
      type P = P0
      val name: String = name0
      val label: String = label0
      val doc: Option[String] = doc0
      val params: Shape[P0] = params0
      val make: P0 => A = make0
    }
  }
  /** no parameters */
  def plain[A](name: String, label: String, doc: Option[String] = None)(a: => A): Action[A] =
    apply[Unit, A](name, label, Shape.unit, doc)(_ => a)
  /** a wire name from a label: lowercased, spaces to dashes, the rest dropped; `x` when nothing is left */
  def nameOf(label: String): String = {
    val n = label.trim.toLowerCase.replaceAll("\\s+", "-").replaceAll("[^a-z0-9_-]", "").dropWhile(c => !c.isLetter)
    if (n.isEmpty) "x" else n
  }
}
