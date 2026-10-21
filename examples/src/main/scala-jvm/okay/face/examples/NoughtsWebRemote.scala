package okay.face.examples

import okay.effects.machine.Machine._
import okay.face.rest.{Linked, RestFace}

/** THE GAME ON THE SERVER: `sbt "examples/runMain okay.face.examples.NoughtsWebRemote [port] [host]"` — the REST face
 * handles the program; `/` is the thin page that plays it from the browser, the library's script, linked by
 * `sbt -Dplatforms=js examplesRemoteJS/fullLinkJS` */
object NoughtsWebRemote {
  def main(args: Array[String]): Unit = run(RestFace().serve(Noughts.games, Args.port(args), Args.host(args),
    page = Some(Linked("examples/remote/.js", "okay-faces-examples-remote")), look = Some(Noughts.light), vocabulary = Noughts.marks.vocabulary))
}
