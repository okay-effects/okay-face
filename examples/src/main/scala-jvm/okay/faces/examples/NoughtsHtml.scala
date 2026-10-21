package okay.faces.examples

import okay.effects.machine.Machine.self
import okay.faces._
import okay.faces.html.HtmlFace

/** THE GAME AS PAGES FROM THE SERVER: `sbt "examples/runMain okay.faces.examples.NoughtsHtml [port] [host]"` — the HTML
 * face handles the program: structural pages, no script, a cell a link */
object NoughtsHtml {
  def main(args: Array[String]): Unit =
    run(HtmlFace.serve(Noughts.games, Args.port(args), Args.host(args), look = Some(Noughts.look)))
}

/** the port and the host from the arguments: `[port] [host]`, `8090` and the loopback by default; `0.0.0.0` opens to
 * the network */
object Args {
  def port(args: Array[String]): Int = args.headOption.filter(_.forall(_.isDigit)).map(_.toInt).getOrElse(8090)
  def host(args: Array[String]): String = args.drop(1).headOption.getOrElse("127.0.0.1")
}
