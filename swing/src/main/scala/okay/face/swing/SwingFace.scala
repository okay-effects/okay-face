package okay.face.swing

import java.awt.{BorderLayout, Component, Font, GridLayout}
import javax.swing._
import javax.swing.table.DefaultTableModel
import okay.effects.Runner
import okay.effects.json.Json
import okay.effects.shape.{Problems, Schema}
import okay.face.{Action, Chosen, Faces, Look, Rejected, Role, Scenario, Step, View}
import scala.collection.mutable

/**
 * THE SWING FACE: a step machine as a Swing panel. The view drawn as components, a plain action a button, an action with
 * parameters a form drawn from its erased schema; a click is `resume`, the rest run, the panel rebuilt. Back
 * re-resumes the previous `Waiting`. `panel` is the face; `window` puts it in a frame on the event dispatch thread.
 * The rest of a step runs by the `Runner`: the row is done before the face. With a `Look`, the components are dressed
 * by their roles (`Looks`) after each rebuild; without one, the platform's look and feel stands.
 * `F` is the implementation's singleton type (`Machine.type`): the faces come implicitly, from `Faces.default` over the
 * implicit implementation, and the `Singleton` bound keeps the type exact when Scala 3 infers it — `new SwingFace()`
 * with `import okay.effects.machine.Machine._`, on both compilers.
 */
class SwingFace[F <: Runner with Singleton](implicit val faces: Faces[F]) {
  import faces.fx._
  type M[X] = X ! okay.effects.Void

  final case class Shown[A](told: Vector[View], step: Step[A, M], history: List[Step.Waiting[A, M]], problems: Problems)

  /** the face as a panel, rebuilt at each step; `done` with the value at the end */
  def panel[A](first: Step[A, M] ! okay.effects.Void, done: A => Unit = (_: A) => (), look: Option[Look] = None, back: Boolean = true): JPanel = {
    val root = new JPanel(new BorderLayout)
    root.setName(Role.Face.cls)
    def settle(step: Step[A, M], told: Vector[View], history: List[Step.Waiting[A, M]]): Shown[A] = step match {
      case Step.Told(view, next) => settle(run(next), told :+ view, history)
      case other => Shown(told, other, history, Nil)
    }
    var current: Option[Step[A, M]] = None
    var unlisten: () => Unit = () => ()
    /** the window grows with its content, within the screen, and never shrinks: a step that adds a line adds no scrollbar */
    def grow(): Unit = Option(SwingUtilities.getWindowAncestor(root)).foreach { w =>
      val viewport = Option(SwingUtilities.getAncestorOfClass(classOf[JViewport], root)).map(_.asInstanceOf[JViewport])
      viewport.foreach { v =>
        val need = root.getPreferredSize
        val screen = java.awt.Toolkit.getDefaultToolkit.getScreenSize
        val h = math.min(w.getHeight - v.getHeight + need.height + 8, screen.height - 120)
        val wd = math.min(w.getWidth - v.getWidth + need.width + 8, screen.width - 80)
        if (h > w.getHeight || wd > w.getWidth) w.setSize(math.max(wd, w.getWidth), math.max(h, w.getHeight))
      }
    }
    def show(s: Shown[A]): Unit = {
      current = Some(s.step)
      root.removeAll()
      root.add(render(s, show, settle, back), BorderLayout.CENTER)
      look.foreach(l => Looks(l)(root))
      root.revalidate(); root.repaint()
      grow()
      unlisten()
      s.step match {
        case Step.Done(a) => done(a)
        case w: Step.Waiting[A, M] @unchecked =>
          // the step's event: when it fires and this is still the step, the rest runs on the event dispatch thread as a click's would
          unlisten = w.listen(next => SwingUtilities.invokeLater(new Runnable { def run(): Unit = if (current.exists(_ eq w)) show(settle(faces.fx.run(next), Vector.empty, w :: s.history)) }))
        case _ => ()
      }
    }
    show(settle(run(first), Vector.empty, Nil))
    root
  }

  /** the face in a window, on the event dispatch thread; with `pages` (core's `Navigation` in memory) the window's title
   * follows the program's current page */
  def window[A](title: String, first: Step[A, M] ! okay.effects.Void, done: A => Unit = (_: A) => (),
                pages: Option[(okay.face.Pages[_], Any => String)] = None, look: Option[Look] = None, back: Boolean = true,
                closed: () => Unit = () => sys.exit(0)): Unit =
    SwingUtilities.invokeLater(new Runnable { def run(): Unit = {
      val frame = new JFrame(title)
      pages.foreach { case (ps, name) => ps.asInstanceOf[okay.face.Pages[Any]].onChange(p => frame.setTitle(s"$title — ${name(p)}")) }
      frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
      frame.addWindowListener(new java.awt.event.WindowAdapter { override def windowClosed(e: java.awt.event.WindowEvent): Unit = closed() })
      frame.getContentPane.add(new JScrollPane(Views.fitting(panel(first, done, look, back)), ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER))
      // the window sized to its content, within the screen; it grows no further — a longer dialogue scrolls
      frame.pack()
      val screen = java.awt.Toolkit.getDefaultToolkit.getScreenSize
      frame.setSize(math.max(480, math.min(frame.getWidth + 24, screen.width - 80)), math.max(360, math.min(frame.getHeight + 24, screen.height - 120)))
      frame.setLocationRelativeTo(null); frame.setVisible(true)
    } })

  private def render[A](s: Shown[A], show: Shown[A] => Unit, settle: (Step[A, M], Vector[View], List[Step.Waiting[A, M]]) => Shown[A], back: Boolean): JComponent = {
    val box = new JPanel; box.setLayout(new BoxLayout(box, BoxLayout.Y_AXIS))
    s.told.foreach(v => box.add(named(Views.component(v), Role.Told.cls)))
    s.step match {
      case Step.Done(a) => box.add(label(s"done: $a", "okay-done"))
      case Step.Told(_, _) => ()
      case w: Step.Waiting[A, M] @unchecked =>
        def choose(chosen: Chosen): Unit = w.resume(chosen) match {
          case Left(Rejected(ps)) => show(s.copy(problems = ps))
          case Right(next) => show(settle(run(next), Vector.empty, w :: s.history))
        }
        val ref: Option[String => Unit] = if (w.actions.exists(_.name == "ref")) Some(key => choose(Chosen("ref", Json.obj("key" -> Json.Str(key))))) else None
        box.add(Views.component(w.view, ref))
        if (s.problems.nonEmpty) { val p = new JPanel(new GridLayout(0, 1)); p.setName(Role.Problems.cls); s.problems.foreach(pr => p.add(label(pr.render, Role.Problems.cls))); box.add(p) }
        val actions = new JPanel; actions.setLayout(new BoxLayout(actions, BoxLayout.Y_AXIS)); actions.setName(Role.Actions.cls)
        w.actions.filterNot(_.name == "ref").foreach(a => actions.add(Forms.action(a, choose)))
        if (back && s.history.nonEmpty) {
          val b = new JButton("Back"); b.setName(Role.Back.cls)
          b.addActionListener(_ => show(Shown(Vector.empty, s.history.head, s.history.tail, Nil)))
          actions.add(b)
        }
        box.add(actions)
    }
    box
  }
  private def label(text: String, name: String): JLabel = { val l = new JLabel(text); l.setName(name); l }
  private def named[C <: JComponent](c: C, name: String): C = { if (c.getName == null) c.setName(name); c }
}

/** a `View` as Swing components */
object SwingFace {
  /** A SCENARIO IN A WINDOW, whole: `run(SwingFace.handle(game))` — the window titled, dressed and backed as the scenario
   * says; the program ends with the dialogue's result, or with `None` when the window is closed first */
  def handle[F <: Runner with Singleton, A](s: Scenario[F, A]): s.faces.fx.![Option[A], s.faces.fx.Pure] = {
    import s.faces.fx._
    val face = new SwingFace[F]()(s.faces)
    // the scenario's steps are over `s.faces`, the face's over `face.faces`: one value, two paths the checker cannot join
    val first = s.steps.asInstanceOf[face.M[Step[A, face.M]]]
    pure(()).flatMap { _ =>
      val outcome = new java.util.concurrent.LinkedBlockingQueue[Option[A]](1)
      face.window(s.title, first, (a: A) => outcome.offer(Some(a)), look = s.look, back = s.back, closed = () => outcome.offer(None))
      pure[Option[A], Pure](outcome.take())
    }
  }
}

object Views {
  /** the face in a scroll pane: as wide as the window, scrolling down only */
  def fitting(c: JComponent): JComponent = {
    val p = new JPanel(new BorderLayout) with Scrollable {
      def getPreferredScrollableViewportSize: java.awt.Dimension = getPreferredSize
      def getScrollableUnitIncrement(r: java.awt.Rectangle, o: Int, d: Int): Int = 16
      def getScrollableBlockIncrement(r: java.awt.Rectangle, o: Int, d: Int): Int = r.height
      def getScrollableTracksViewportWidth: Boolean = true
      def getScrollableTracksViewportHeight: Boolean = false
    }
    p.add(c, BorderLayout.NORTH); p
  }
  def component(v: View): JComponent = component(v, None)
  /** `ref`: what a `Ref` does when the step offers the action `ref` — a button, and a table of such a grid of them */
  def component(v: View, ref: Option[String => Unit]): JComponent = v match {
    case View.Empty => new JPanel
    case View.Text(s) => named(wrapped(s), Role.Text.cls)
    case View.Heading(level, s) =>
      val l = new JLabel(s); l.setFont(l.getFont.deriveFont(Font.BOLD, l.getFont.getSize2D * (level match { case 1 => 1.6f; case 2 => 1.3f; case _ => 1.1f }))); named(l, Role.Heading(level).cls)
    case View.Notice(level, s) => named(new JLabel(s"${level.toString.toLowerCase}: $s"), Role.Notice(level).cls)
    case View.Fields(rows) =>
      val p = new JPanel(new GridLayout(0, 2, 8, 2))
      rows.foreach { case (k, x) => p.add(named(new JLabel(k + ":"), Role.Label.cls)); p.add(component(x, ref)) }
      named(p, Role.Fields.cls)
    // a table with no column names is a GRID of cells — a board — in the middle, its cells square: as wide as the window
    // allows, up to one and a half times a cell's own height; so is one of live refs
    case View.Table(columns, rows) if columns.forall(_.isEmpty) || (ref.isDefined && rows.exists(_.exists(hasRef))) =>
      val cols = rows.headOption.fold(1)(_.size) max 1
      val holder = new JPanel(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 0, 0)); holder.setOpaque(false)
      val grid = new JPanel(new GridLayout(rows.size, cols, 4, 4)) {
        override def getPreferredSize: java.awt.Dimension = {
          val own = super.getPreferredSize
          val cap = (own.height.toDouble / rows.size * 1.5).toInt
          val fit = if (holder.getWidth > 0) holder.getWidth / cols else cap
          val side = math.max(own.width / cols, math.min(fit, cap))
          new java.awt.Dimension(side * cols, side * rows.size)
        }
      }
      holder.addComponentListener(new java.awt.event.ComponentAdapter { override def componentResized(e: java.awt.event.ComponentEvent): Unit = grid.revalidate() })
      rows.foreach(_.foreach(c => grid.add(cell(c, ref)))); named(grid, Role.Table.cls)
      holder.add(grid); holder
    case View.Table(columns, rows) =>
      val model = new DefaultTableModel(rows.map(_.map(c => text(c): AnyRef).toArray).toArray, columns.map(c => c: AnyRef).toArray) {
        override def isCellEditable(r: Int, c: Int): Boolean = false
      }
      val t = new JTable(model); t.setName(Role.Table.cls)
      val sp = new JScrollPane(t); sp.setPreferredSize(new java.awt.Dimension(400, math.min(300, 24 * (rows.size + 2)))); sp
    case View.Items(items) =>
      val list = new JList[String](items.map(text).toArray); named(list, Role.Items.cls)
    case View.Group(title, children) =>
      val p = new JPanel; p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS))
      title.foreach(t => p.setBorder(BorderFactory.createTitledBorder(t)))
      children.foreach(c => p.add(component(c, ref))); named(p, Role.Group.cls)
    case View.Ref(key, l) => ref match {
      case Some(r) => val b = new JButton(l); b.setName(Role.Ref.cls); b.setToolTipText(key); b.addActionListener((_: java.awt.event.ActionEvent) => r(key)); b
      case None => named(new JLabel(s"$l [$key]"), Role.Ref.cls)
    }
    case View.Datum(json, _) => val a = new JTextArea(Json.pretty(json)); a.setEditable(false); a.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 12)); named(a, Role.Datum.cls)
    case View.Masked(hint) => named(new JLabel(hint), Role.Masked.cls)
    case View.Marked(mark, x) => val c = component(x, ref); c.setName(Role.Mark(mark).cls); c   // the mark's rule dresses it, not its own
  }
  private def hasRef(v: View): Boolean = v match { case View.Ref(_, _) => true; case View.Marked(_, x) => hasRef(x); case _ => false }
  /** a cell of a grid: a live Ref its button, a Ref not live its label, anything else a label — in the middle, square;
   * the mark's name kept for the look */
  private def cell(v: View, ref: Option[String => Unit]): JComponent = v match {
    case View.Marked(mark, x) => val c = cell(x, ref); c.setName(Role.Mark(mark).cls); c
    case View.Ref(key, l) if ref.isDefined =>
      val b = new JButton(l)
      b.setName(Role.Ref.cls); b.setToolTipText(key); b.setHorizontalAlignment(SwingConstants.CENTER); b.addActionListener((_: java.awt.event.ActionEvent) => ref.get(key)); b
    case other =>
      val l = new JLabel(other match { case View.Ref(_, lbl) => lbl; case _ => text(other) }, SwingConstants.CENTER)
      named(l, other match { case View.Ref(_, _) => Role.Ref.cls; case _ => Role.Text.cls })
  }
  /** a view in one line */
  def text(v: View): String = v match {
    case View.Empty => ""
    case View.Text(s) => s
    case View.Heading(_, s) => s
    case View.Notice(_, s) => s
    case View.Fields(rows) => rows.map { case (k, x) => s"$k: ${text(x)}" }.mkString(", ")
    case View.Table(_, rows) => rows.map(_.map(text).mkString(" ")).mkString(" / ")
    case View.Items(items) => items.map(text).mkString(", ")
    case View.Group(t, children) => (t.toList ++ children.map(text)).mkString(" ")
    case View.Ref(key, l) => s"$l [$key]"
    case View.Datum(json, _) => Json.print(json)
    case View.Masked(hint) => hint
    case View.Marked(_, x) => text(x)
  }
  private def wrapped(s: String): JTextArea = { val a = new JTextArea(s); a.setEditable(false); a.setLineWrap(true); a.setWrapStyleWord(true); a.setOpaque(false); a }
  private def named[C <: JComponent](c: C, name: String): C = { c.setName(name); c }
}

/** an action as a button, or as a form drawn from its parameters' erased schema; what is typed becomes JSON the
 * action's shape decodes — the face does not validate, the shape does */
object Forms {
  def action[A](a: Action[A], choose: Chosen => Unit): JComponent = a.params.erased match {
    case Schema.Record(fields) if fields.isEmpty =>
      val b = new JButton(a.label); b.setName("action:" + a.name)
      b.addActionListener(_ => choose(Chosen.plain(a.name))); b
    case schema =>
      val inputs = new Inputs
      val p = new JPanel; p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS)); p.setName("form:" + a.name)
      p.setBorder(BorderFactory.createTitledBorder(a.label))
      a.doc.foreach(d => { val l = new JLabel(d); l.setName(Role.Doc.cls); p.add(l) })
      p.add(inputs.of(Nil, schema))
      val submit = new JButton(a.label); submit.setName("submit:" + a.name)
      submit.addActionListener(_ => choose(Chosen(a.name, inputs.collect(Nil, schema))))
      p.add(submit); p
  }

  final class Inputs {
    private val texts = mutable.Map.empty[List[String], JTextField]
    private val checks = mutable.Map.empty[List[String], JCheckBox]
    private val combos = mutable.Map.empty[List[String], JComboBox[String]]
    private val areas = mutable.Map.empty[List[String], JTextArea]
    private val items = mutable.Map.empty[List[String], () => Vector[Int]]
    private def name(path: List[String]) = "f" + path.map("-" + _).mkString

    def of(path: List[String], schema: Schema, field: Option[Schema.Field] = None): JComponent = schema match {
      case Schema.Record(fs) =>
        val p = new JPanel(new GridLayout(0, 2, 8, 4))
        field.flatMap(_.label).orElse(field.map(_.name)).foreach(l => p.setBorder(BorderFactory.createTitledBorder(l)))
        fs.foreach { f => val l = new JLabel(f.label.getOrElse(f.name) + ":"); l.setName(Role.Label.cls); p.add(l); p.add(of(path :+ f.name, f.schema, Some(f))) }
        p
      case Schema.Described(of0, label0, doc) =>
        of(path, of0, Some(field.getOrElse(Schema.Field(path.lastOption.getOrElse(""), of0, None, None, false, None)).copy(label = Some(label0), doc = doc)))
      case Schema.Optional(of0) => of(path, of0, field)
      case Schema.Choice(options) =>
        val c = new JComboBox[String](options.toArray); c.setName(name(path)); combos(path) = c; c
      case Schema.OneOf(cases) =>
        val c = new JComboBox[String](cases.map(_._1).toArray); c.setName(name(path)); combos(path) = c
        val cards = new JPanel(new java.awt.CardLayout)
        cases.foreach { case (n, cs) => cards.add(of(path :+ n, cs), n) }
        c.addActionListener(_ => cards.getLayout.asInstanceOf[java.awt.CardLayout].show(cards, c.getSelectedItem.asInstanceOf[String]))
        val p = new JPanel; p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS)); p.add(c); p.add(cards); p
      case Schema.Many(of0, min, max) if !okay.face.FormJson.leafish(of0) =>
        // a list of records: an item added by a button, removed by one; each item's fields by index
        val list = new JPanel; list.setLayout(new BoxLayout(list, BoxLayout.Y_AXIS)); list.setName(name(path))
        var next = 0; var ids = Vector.empty[Int]
        items(path) = () => ids
        def add(): Unit = if (!max.exists(ids.size >= _)) {
          val i = next; next += 1; ids = ids :+ i
          val item = new JPanel; item.setLayout(new BoxLayout(item, BoxLayout.Y_AXIS)); item.setName(name(path :+ i.toString))
          item.add(of(path :+ i.toString, of0))
          val remove = new JButton("Remove"); remove.setName("remove:" + name(path :+ i.toString))
          remove.addActionListener(_ => { ids = ids.filterNot(_ == i); list.remove(item); list.revalidate(); list.repaint() })
          item.add(remove); list.add(item); list.revalidate(); list.repaint()
        }
        (0 until (min max 0)).foreach(_ => add())
        val addButton = new JButton("Add"); addButton.setName("add:" + name(path)); addButton.addActionListener(_ => add())
        val p = new JPanel; p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS))
        field.flatMap(_.label).orElse(field.map(_.name)).foreach(l => p.setBorder(BorderFactory.createTitledBorder(l)))
        p.add(list); p.add(addButton); p
      case Schema.Many(_, _, _) =>
        val a = new JTextArea(3, 20); a.setName(name(path)); a.setToolTipText("one per line"); areas(path) = a; new JScrollPane(a)
      case Schema.Bool =>
        val c = new JCheckBox(); c.setName(name(path)); checks(path) = c
        field.flatMap(_.default).foreach { case Json.Bool(b) => c.setSelected(b); case _ => () }; c
      case leaf =>
        val t = if (field.exists(_.secret)) new JPasswordField(16) else new JTextField(16)
        t.setName(name(path)); texts(path) = t
        field.flatMap(_.default).foreach(d => t.setText(d match { case Json.Str(s) => s; case other => Json.print(other) }))
        field.flatMap(_.doc).foreach(t.setToolTipText); t
    }

    def collect(path: List[String], schema: Schema): Json = schema match {
      case Schema.Record(fs) => Json.Obj(fs.flatMap(f => collect(path :+ f.name, f.schema) match { case Json.Null => None; case v => Some(f.name -> v) }))
      case Schema.Described(of0, _, _) => collect(path, of0)
      case Schema.Optional(of0) => collect(path, of0)
      case Schema.Choice(_) => combos.get(path).map(c => Json.Str(c.getSelectedItem.asInstanceOf[String])).getOrElse(Json.Null)
      case Schema.OneOf(cases) => combos.get(path).map(_.getSelectedItem.asInstanceOf[String]).flatMap(n => cases.find(_._1 == n)) match {
        case Some((n, cs)) => collect(path :+ n, cs) match {
          case o: Json.Obj if Schema.isRecord(cs) => ("type" -> Json.Str(n)) +: o
          case v => Json.Obj.of("type" -> Json.Str(n), "value" -> v)
        }
        case None => Json.Null
      }
      case Schema.Many(of0, _, _) if items.contains(path) => Json.Arr(items(path)().map(i => collect(path :+ i.toString, of0)))
      case Schema.Many(of0, _, _) => areas.get(path).map(a => Json.Arr(a.getText.split("\n").toVector.map(_.trim).filter(_.nonEmpty).map(typed(of0, _)))).getOrElse(Json.Null)
      case Schema.Bool => checks.get(path).map(c => Json.Bool(c.isSelected)).getOrElse(Json.Null)
      case leaf => texts.get(path).map(_.getText).filter(_.nonEmpty).map(typed(leaf, _)).getOrElse(Json.Null)
    }
    private def typed(schema: Schema, s: String): Json = schema match {
      case Schema.Integer(_) | Schema.Number(_) => scala.util.Try(BigDecimal(s.trim)).toOption.fold[Json](Json.Str(s))(Json.Num(_))
      case Schema.Bool => Json.Bool(Set("true", "yes", "y", "on").contains(s.trim.toLowerCase))
      case Schema.Described(of0, _, _) => typed(of0, s)
      case _ => Json.Str(s)
    }
  }
}
