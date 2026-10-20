package okay.face

/** THE LOOK as data: complete over the roles, compiled to a stylesheet that says only what the rules say */
class StyleSuite extends munit.FunSuite {
  test("every role has a name, two classes at most, and is found back by its class") {
    Role.all.foreach { r =>
      assert(r.names.nonEmpty && r.names.size <= 2 && r.classes.startsWith("okay-"), r.toString)
      assertEquals(Role.byClass(r.cls), r)
    }
    assertEquals(Role.Heading(1).classes, "okay-heading okay-heading-1")
    assertEquals(Role.Notice(View.Notice.Problem).cls, "okay-notice-problem")
    assertEquals(Role.all.size, Role.all.distinct.size)
  }
  test("the default style says something for the roles a dialogue is read by") {
    val said = Role.all.filter(r => !Style.default(r).isEmpty)
    assert(said.contains(Role.Heading(1)) && said.contains(Role.Notice(View.Notice.Problem)) && said.contains(Role.Action) && said.contains(Role.Masked))
    assertEquals(Style.plain(Role.Heading(1)), Rule())
    assertEquals(Style.default.adjust(Role.Text)(_.copy(mono = true))(Role.Text).mono, true)
  }
  test("a colour's hex round trip; the two themes differ in every tone") {
    assertEquals(Color("#0969da").hex, "#0969da")
    assertEquals(Color(Color(1, 2, 3).hex), Color(1, 2, 3))
    Tone.all.foreach(t => assertNotEquals(Theme.light.palette(t), Theme.dark.palette(t)))
  }
  test("the stylesheet: a custom property per tone, a block per role that has a rule, in tokens not colours") {
    val css = Css(Look.default)
    Tone.all.foreach(t => assert(css.contains(s"--okay-${t.name}:${Theme.light.palette(t).hex}"), t.name))
    Role.all.filter(r => !Style.default(r).isEmpty).foreach(r => assert(css.contains(s".${r.cls}{"), r.cls))
    assert(css.contains(".okay-heading-1{font-weight:bold;font-size:1.6em}"), css)
    assert(css.contains(".okay-notice-problem{color:var(--okay-problem);border:1px solid var(--okay-problem);padding:calc(1 * var(--okay-space))}"), css)
    assert(css.contains(".okay-actions{display:flex;flex-wrap:wrap;align-items:center;gap:calc(2 * var(--okay-space))}"), css)
    assert(css.contains(".okay-table th,.okay-table td{border:1px solid var(--okay-border);padding:calc(1 * var(--okay-space))}"), css)
    assert(!css.contains(".okay-text{"), "a role with no rule has no block")
    assert(!Css(Look.default).split("\n").drop(1).exists(_.contains("#")), "rules name tones, the palette alone has colours")
    assert(Css(Look.dark).contains("--okay-bg:#0d1117"))
  }
}
