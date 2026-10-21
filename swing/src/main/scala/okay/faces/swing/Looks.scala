package okay.faces.swing

import java.awt.{Color => AwtColor, Component, Container, Font}
import javax.swing.{AbstractButton, BorderFactory, JComponent, JLabel, SwingConstants}
import okay.faces.{Family, Look, Role, Rule, Tone}

/** SWING'S INTERPRETER of a look: the components of a face, found by their names — the roles' classes the face set —
 * dressed by their rules: the font's family, size and weight, the foreground and background tones, a line border, a
 * padding. Layout stays the face's. Buttons named `action:`, `submit:` are actions and `form:` a form */
final class Looks(look: Look) {
  private val base: Float = look.theme.size.toFloat
  private def family(mono: Boolean): String =
    if (mono) Font.MONOSPACED else look.theme.font match { case Family.System => Font.SANS_SERIF; case Family.Serif => Font.SERIF; case Family.Mono => Font.MONOSPACED }
  private def color(t: Tone): AwtColor = { val c = look.theme.palette(t); new AwtColor(c.r, c.g, c.b) }

  def roleOf(c: Component): Option[Role] = Option(c.getName).flatMap { n =>
    if (n.startsWith("action:") || n.startsWith("submit:")) Some(Role.Action)
    else if (n.startsWith("form:")) Some(Role.Form)
    else Role.ofClass(n)
  }

  /** the rule on one component */
  def dress(c: JComponent, r: Rule): Unit = {
    val style = if (r.bold) Font.BOLD else Font.PLAIN
    c.setFont(new Font(family(r.mono), style, (base * r.size).round.toInt))
    r.color.foreach(t => c.setForeground(color(t)))
    r.background.foreach { t => c.setBackground(color(t)); c.setOpaque(true) }
    if (r.centered) c match {
      case l: JLabel => l.setHorizontalAlignment(SwingConstants.CENTER)
      case b: AbstractButton => b.setHorizontalAlignment(SwingConstants.CENTER)
      case _ => c.setAlignmentX(java.awt.Component.CENTER_ALIGNMENT)
    }
    val pad = r.pad * look.theme.space
    val inner = if (pad > 0) Some(BorderFactory.createEmptyBorder(pad, pad, pad, pad)) else None
    val line = r.border.map(t => BorderFactory.createLineBorder(color(t)))
    (line, inner) match {
      case (Some(l), Some(i)) => c.setBorder(BorderFactory.createCompoundBorder(l, i))
      case (Some(l), None) => c.setBorder(l)
      case (None, Some(i)) => c match { case _: JLabel | _: AbstractButton => c.setBorder(i); case _ if c.getBorder == null => c.setBorder(i); case _ => () }
      case (None, None) => ()
    }
  }

  /** the whole tree, by role; what has no role gets the face's font and tones, so the look is one */
  def apply(root: JComponent): Unit = {
    val face = look.style(Role.Face)
    def go(c: Component): Unit = {
      c match {
        case j: JComponent => roleOf(j) match {
          case Some(role) => dress(j, look.style(role))
          case None => j.setFont(new Font(family(mono = false), Font.PLAIN, base.round.toInt)); face.color.foreach(t => j.setForeground(color(t)))
        }
        case _ => ()
      }
      c match { case p: Container => p.getComponents.foreach(go); case _ => () }
    }
    go(root)
  }
}
object Looks { def apply(look: Look): Looks = new Looks(look) }
