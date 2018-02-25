/*
 * Copyright (c) 2017-2018 The Typelevel Cats-effect Project Developers
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cats.effect
package internals

import java.util.concurrent.{Executors, ScheduledExecutorService, ThreadFactory}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

private[effect] abstract class TimerImplementation {
  /**
   * Given an `ExecutionContext`, builds a [[Timer]] for any data type
   * that has an [[Async]] instance.
   *
   * Note that the actual scheduling with a delay happens on
   * an internal `ScheduledExecutorService`, but the execution of
   * the tick and the bind continuation of the async data type
   * gets shifted on the `ec` execution context.
   *
   * @param ec is the Scala `ExecutionContext` that will get used
   *        for asynchronous execution of tasks
   */
  def buildFor[F[_]](implicit F: Async[F], ec: ExecutionContext): Timer[F] =
    buildFor(ec, scheduler)(F)

  /**
   * Given an Scala `ExecutionContext` and a `ScheduledExecutorService`
   * builds a [[Timer]] for any data type that has an [[Async]] instance.
   *
   * @param ec is the Scala `ExecutionContext` that will get used
   *        for asynchronous execution of tasks
   * @param sc is the scheduler executor used to schedule delayed
   *        ticks
   */
  def buildFor[F[_]](ec: ExecutionContext, sc: ScheduledExecutorService)
    (implicit F: Async[F]): Timer[F] =
    new AsyncTimer[F](scheduler, ec)

  /**
   * Generic, JVM-based `Timer` implementation.
   */
  private final class AsyncTimer[F[_]](
    sc: ScheduledExecutorService, ec: ExecutionContext)
    (implicit F: Async[F]) extends Timer[F] {

    override def currentTimeMillis: F[Long] =
      F.delay(System.currentTimeMillis())

    override def sleep(timespan: FiniteDuration): F[Unit] =
      F.cancelable { cb =>
        val f = sc.schedule(new ShiftTick(cb, ec), timespan.length, timespan.unit)
        IO(f.cancel(false))
      }

    override def shift: F[Unit] =
      F.async(cb => ec.execute(new Tick(cb)))
  }

  private lazy val scheduler: ScheduledExecutorService =
    Executors.newScheduledThreadPool(2, new ThreadFactory {
      def newThread(r: Runnable): Thread = {
        val th = new Thread(r)
        th.setName("cats-effect")
        th.setDaemon(true)
        th
      }
    })

  private final class ShiftTick(
    cb: Either[Throwable, Unit] => Unit, ec: ExecutionContext)
    extends Runnable {
    def run() = {
      // Shifts actual execution on our `ExecutionContext`, because
      // the scheduler is in charge only of ticks and the execution
      // needs to shift because the tick might continue with whatever
      // bind continuation is linked to it, keeping the current thread
      // occupied
      ec.execute(new Tick(cb))
    }
  }

  private final class Tick(cb: Either[Throwable, Unit] => Unit)
    extends Runnable {
    def run() = cb(Callback.rightUnit)
  }
}
