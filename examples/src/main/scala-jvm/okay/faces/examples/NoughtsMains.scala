package okay.faces.examples

import okay.effects.machine.Machine.self
import okay.faces._
import okay.faces.html.HtmlFace
import okay.faces.mcp.McpFace
import okay.faces.rest.{Linked, RestFace, Shell}
import okay.faces.swing.SwingFace
import okay.faces.term.Terminal

/** NOUGHTS AND CROSSES ON EVERY FACE, each main one line: a face is a handler of the one effect, given the program and
 * nothing else; the implementation is said once, by the import.
 * `sbt "examples/runMain okay.faces.examples.NoughtsSwing"` and so on; the servers take `[port] [host]` */
object NoughtsSwing {
  def main(args: Array[String]): Unit = run(SwingFace.handle(Noughts.games, look = Some(Noughts.look)))
}
/** the real console handles the program; the same program the test face plays by script */
object NoughtsTerminal {
  def main(args: Array[String]): Unit = { run(Terminal.console(Noughts.games)); println("bye") }
}
/** the program served over the Model Context Protocol on standard input and output: an agent plays by the cells as
 * tools, told the marks */
object NoughtsMcp {
  def main(args: Array[String]): Unit = run(McpFace.stdio(Noughts.games, vocabulary = Noughts.marks.vocabulary))
}
/** the game as pages from the server: structural pages, no script, a cell a link */
object NoughtsHtml {
  def main(args: Array[String]): Unit =
    run(HtmlFace.serve(Noughts.games, Args.port(args), Args.host(args), look = Some(Noughts.look)))
}
/** THE GAME IN THE BROWSER: the server serves the page and `Pages`' script, linked by
 * `sbt -Dplatforms=js examplesJS/fullLinkJS`; the game runs in the page, the Laminar face handling it there */
object NoughtsWebLocal {
  def main(args: Array[String]): Unit = run(Shell.serve("Noughts and crosses", Args.port(args), Args.host(args),
      Linked("examples/.js", "okay-faces-examples"), look = Some(Noughts.look)))
}
/** THE GAME ON THE SERVER: the REST face handles the program; `/` is the thin page that plays it from the browser,
 * the library's script, linked by `sbt -Dplatforms=js examplesRemoteJS/fullLinkJS` */
object NoughtsWebRemote {
  def main(args: Array[String]): Unit = run(RestFace.serve(Noughts.games, Args.port(args), Args.host(args),
      page = Some(Linked("examples/remote/.js", "okay-faces-examples-remote")), look = Some(Noughts.look),
      vocabulary = Noughts.marks.vocabulary))
}

/** the port and the host from the arguments: `[port] [host]`, `8090` and the loopback by default; `0.0.0.0` opens to
 * the network */
object Args {
  def port(args: Array[String]): Int = args.headOption.filter(_.forall(_.isDigit)).map(_.toInt).getOrElse(8090)
  def host(args: Array[String]): String = args.drop(1).headOption.getOrElse("127.0.0.1")
}
