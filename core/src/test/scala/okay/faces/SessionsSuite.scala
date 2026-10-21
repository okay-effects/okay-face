package okay.faces

/** sessions by id: random ids of 32 hex characters, found and refreshed, forgotten when idle or when too many */
class SessionsSuite extends munit.FunSuite {
  test("random ids: 32 hex characters, no two the same") {
    val ids = (1 to 100).map(_ => Sessions.random())
    assert(ids.forall(_.matches("[0-9a-f]{32}")), ids.head); assertEquals(ids.distinct.size, 100)
  }
  test("counting ids for a test; a session found, updated, removed") {
    val s = new Sessions[String](Sessions.counting("s"))
    assertEquals(s.open("a"), "s1"); assertEquals(s.open("b"), "s2")
    assertEquals(s.get("s1"), Some("a")); s.update("s1", "A"); assertEquals(s.get("s1"), Some("A"))
    assertEquals(s.remove("s2"), Some("b")); assertEquals(s.get("s2"), None); assertEquals(s.size, 1)
  }
  test("idle longer than the ttl, forgotten at the next sweep; a get keeps one alive") {
    var now = 0L
    val s = new Sessions[String](Sessions.counting("s"), ttl = 100, clock = () => now)
    s.open("a"); s.open("b")
    now = 80; s.get("s1")
    now = 150; s.sweep()
    assertEquals(s.get("s1"), Some("a")); assertEquals(s.get("s2"), None)
  }
  test("at most max: the oldest goes when one more is opened") {
    val s = new Sessions[String](Sessions.counting("s"), max = 3)
    s.open("a"); s.open("b"); s.open("c"); s.open("d")
    assertEquals(s.get("s1"), None); assertEquals(s.get("s4"), Some("d")); assertEquals(s.size, 3)
  }
}
