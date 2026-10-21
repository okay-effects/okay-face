package okay.face.html

import java.io.File

/**
 * A LINKED SCALA.JS SCRIPT, where sbt puts it: `dir/target/scala-<version>/<name>-opt/main.js` (`fullLinkJS`), or
 * `-fastopt` (`fastLinkJS`), the full one first, any Scala version; `dir` a Scala.js project's directory (a cross
 * project's `.js`), `name` its artifact name. Read when the page asks for it, so a relink is served without a restart;
 * a script not linked is the reason, served as a 503 by `Shell.handle`
 */
final case class Linked(dir: String, name: String) extends (() => Either[String, String]) {
  def apply(): Either[String, String] = {
    val target = new File(dir, "target")
    val versions = Option(target.listFiles).toList.flatten.filter(d => d.isDirectory && d.getName.startsWith("scala-"))
    val candidates = for { kind <- List("opt", "fastopt"); v <- versions.sortBy(_.getName).reverse } yield new File(v, s"$name-$kind/main.js")
    candidates.find(_.isFile) match {
      case Some(f) => Right(new String(java.nio.file.Files.readAllBytes(f.toPath), "UTF-8"))
      case None => Left(s"no script $name under $dir/target: `sbt -Dplatforms=js <project>/fullLinkJS` links it")
    }
  }
}
