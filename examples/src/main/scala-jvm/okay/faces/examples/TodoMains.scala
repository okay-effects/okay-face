package okay.faces.examples

import okay.effects.machine.Machine.self
import okay.faces._
import okay.faces.html.HtmlFace
import okay.faces.mcp.McpFace
import okay.faces.rest.{Linked, RestFace, Shell}
import okay.faces.swing.SwingFace
import okay.faces.term.Terminal

/** THE LIST ON EVERY FACE, each main one line, as the game's: the faces know nothing of either program.
 * `sbt "examples/runMain okay.faces.examples.TodoSwing"` and so on; the servers take `[port] [host]` */
object TodoSwing {
  def main(args: Array[String]): Unit = run(SwingFace.handle(Todo.keeping, SwingFace.Settings(look = Some(Todo.look))))
}
object TodoTerminal {
  def main(args: Array[String]): Unit = { run(Terminal.console(Todo.keeping)); println("bye") }
}
object TodoMcp {
  def main(args: Array[String]): Unit =
    run(McpFace.stdio(Todo.keeping, McpFace.Settings(vocabulary = Todo.marks.vocabulary)))
}
object TodoHtml {
  def main(args: Array[String]): Unit =
    run(HtmlFace.serve(Todo.keeping, Args.port(args), Args.host(args), HtmlFace.Settings(look = Some(Todo.look))))
}
/** THE LIST IN THE BROWSER: the server serves the page and `TodoLocal`'s script, linked by
 * `sbt -Dplatforms=js examplesTodoJS/fullLinkJS`; the list runs in the page, the Laminar face handling it there */
object TodoWebLocal {
  def main(args: Array[String]): Unit =
    run(Shell.serve("To do", Args.port(args), Args.host(args), Linked("examples/todo/.js", "okay-faces-examples-todo"),
        look = Some(Todo.look)))
}
/** the list on the server, the thin page playing it */
object TodoWebRemote {
  def main(args: Array[String]): Unit = run(RestFace.serve(Todo.keeping, Args.port(args), Args.host(args),
      RestFace.Settings(page = Some(Linked("examples/remote/.js", "okay-faces-examples-remote")),
        look = Some(Todo.look), vocabulary = Todo.marks.vocabulary)))
}
