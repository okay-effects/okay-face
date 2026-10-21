package okay.faces

import scala.concurrent.{ExecutionContext, Future}

/**
 * WHAT MAY HAPPEN WHILE A STEP WAITS: a timer, a server's push, another's change — an event the program offers beside
 * its actions (`offer(view, actions, meanwhile)`), with a value of the offer's own type, so a step is resumed either by
 * the person's choice or by the event, whichever comes first. A `Meanwhile` is a source: `register` a callback, get
 * a way to cancel; made from a `Future`, by hand (`manual`, for a test), or by whatever an application has. A face
 * that can listen (Laminar, Swing, a dialogue on a server) resumes the step when it fires; one that cannot (the
 * terminal: a line read blocks) ignores it, so a program for every face gives the person an action too.
 */
final class Meanwhile[A](val register: (A => Unit) => (() => Unit)) {
  def map[B](f: A => B): Meanwhile[B] = new Meanwhile[B](k => register(a => k(f(a))))
}
object Meanwhile {
  def apply[A](register: (A => Unit) => (() => Unit)): Meanwhile[A] = new Meanwhile[A](register)
  /** the future's value when it comes, unless cancelled before */
  def fromFuture[A](f: Future[A])(implicit ec: ExecutionContext): Meanwhile[A] = Meanwhile { k =>
    var live = true
    f.foreach(a => if (live) k(a))
    () => live = false
  }
  /** fired by hand: a test's, or an application's own bus */
  def manual[A]: Manual[A] = new Manual[A]
  final class Manual[A] {
    private var listeners: List[A => Unit] = Nil
    val meanwhile: Meanwhile[A] = Meanwhile { k =>
      synchronized { listeners = k :: listeners }; () => synchronized { listeners = listeners.filterNot(_ eq k) }
    }
    def fire(a: A): Unit = synchronized(listeners).foreach(_(a))
    def listening: Int = synchronized(listeners.size)
  }
}
