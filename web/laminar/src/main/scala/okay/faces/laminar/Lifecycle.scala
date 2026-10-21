package okay.faces.laminar

import com.raquo.laminar.api.L._
import okay.effects.Runner
import okay.effects.async.{Async, Asynchrony, Running}
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

/** A PROGRAM TIED TO AN ELEMENT'S LIFE: started when the element mounts, cancelled when it unmounts — what it waits on
 * cancelled too (okay-async's cancellation: a timer cleared, a fetch abandoned); its result, a value or a failure, to
 * `done`, unless it was cancelled first */
class Lifecycle[F <: Runner with Singleton](implicit implementation: F, ec: ExecutionContext) {
  val fx: F = implementation
  import fx._
  val asynchrony: Asynchrony[fx.type] = new Asynchrony[fx.type]()(fx, ec)

  def runOnMount[A](p: A ! Async)(done: Either[Throwable, A] => Unit): Modifier[HtmlElement] = {
    var running: Option[Running[A]] = None
    onMountUnmountCallback(
      mount = _ => {
        val r = asynchrony.start(p)
        running = Some(r)
        r.future.onComplete { case Success(a) => if (running.contains(r)) done(Right(a)); case Failure(e) => if (running.contains(r)) done(Left(e)) }
      },
      unmount = _ => { running.foreach(_.cancel()); running = None })
  }
}

object Lifecycle {
  def apply()(implicit fx: Runner, ec: ExecutionContext): Lifecycle[fx.type] = {
    implicit val exact: fx.type = fx
    new Lifecycle[fx.type]
  }
}
