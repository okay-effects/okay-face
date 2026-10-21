package okay.faces.term

import okay.faces.{Look, Paint, Role, Rule}

/** THE TERMINAL'S INTERPRETER of a look: a role's rule as SGR — bold, the colour of its tone and of its background,
 * 24-bit — around the text, reset after. Off by default: a terminal face paints only when given `Ansi(look)`, so pipes
 * stay plain */
object Ansi {
  def apply(look: Look): Paint = new Paint {
    def apply(role: Role, s: String): String = {
      val r = look.style(role)
      val codes = sgr(r, look)
      if (codes.isEmpty || s.isEmpty) s else s"\u001b[${codes.mkString(";")}m$s\u001b[0m"
    }
  }
  def sgr(r: Rule, look: Look): List[String] =
    (if (r.bold) List("1") else Nil) ++
      r.color.map(t => { val c = look.theme.palette(t); s"38;2;${c.r};${c.g};${c.b}" }).toList ++
      r.background.map(t => { val c = look.theme.palette(t); s"48;2;${c.r};${c.g};${c.b}" }).toList
}
