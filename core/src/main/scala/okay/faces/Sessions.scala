package okay.faces

/**
 * SESSIONS BY ID, for a face that serves many counterparts (HTML, REST, MCP over HTTP): opened with a fresh id, found
 * by it, and FORGOTTEN — one idle longer than `ttl` milliseconds, and the oldest beyond `max` of them — so a server
 * that runs for long does not keep every visitor forever. The ids are random by default (`Sessions.random`, 128 bits
 * as hex), so one is not guessed from another; a test gives `Sessions.counting("s")` and knows them. Safe to share
 * between threads.
 */
final class Sessions[D](ids: () => String = Sessions.random, ttl: Long = Sessions.hour, max: Int = 1000,
  clock: () => Long = () => System.currentTimeMillis()) {
  private val held = scala.collection.mutable.LinkedHashMap.empty[String, (D, Long)]
  /** a session opened: its id */
  def open(d: D): String = synchronized {
    sweep(); val id = ids(); held(id) = (d, clock()); id
  }
  /** the session, if it is there — and it lives on from now */
  def get(id: String): Option[D] = synchronized {
    held.get(id).map { case (d, _) => held.remove(id); held(id) = (d, clock()); d }
  }
  def update(id: String, d: D): Unit = synchronized { held.remove(id); held(id) = (d, clock()) }
  /** the session removed: it, if it was there */
  def remove(id: String): Option[D] = synchronized { held.remove(id).map(_._1) }
  def size: Int = synchronized(held.size)
  /** the idle ones gone, then the oldest beyond `max`; the map is kept in order of last use, oldest first */
  def sweep(): Unit = synchronized {
    val now = clock()
    held.filterInPlace { case (_, (_, at)) => now - at <= ttl }
    while (held.size >= max && held.nonEmpty) held.remove(held.head._1)
  }
}
object Sessions {
  val hour: Long = 60L * 60 * 1000
  /** 128 random bits as 32 hex characters */
  val random: () => String = () => {
    val r = new scala.util.Random()
    f"${r.nextLong()}%016x${r.nextLong()}%016x"
  }
  /** `prefix1`, `prefix2`, …: for a test that names its sessions */
  def counting(prefix: String): () => String = { var n = 0; () => { n += 1; s"$prefix$n" } }
}
