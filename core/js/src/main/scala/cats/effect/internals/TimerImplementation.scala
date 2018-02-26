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

import scala.concurrent.duration.FiniteDuration
import scala.scalajs.js

private[effect] abstract class TimerImplementation {
  import TimerImplementation._

  /**
   * Returns a generic [[Timer]] for any `F` data type that
   * implements [[Async]].
   *
   * This is the JavaScript version, based on the standard `setTimeout`
   * and `setImmediate` where available.
   */
  def async[F[_]](implicit F: Async[F]): Timer[F] =
    new AsyncTimer[F]()
}

private[internals] object TimerImplementation {
  /**
   * Generic, JavaScript-based `Timer` implementation.
   */
  final class AsyncTimer[F[_]](implicit F: Async[F])
    extends Timer[F] {

    override def currentTimeMillis: F[Long] =
      F.delay(System.currentTimeMillis())

    override def sleep(timespan: FiniteDuration): F[Unit] =
      F.cancelable { cb =>
        val task = setTimeout(timespan.toMillis, new Tick(cb))
        IO(clearTimeout(task))
      }

    override def shift: F[Unit] =
      F.async(cb => setImmediate(new Tick(cb)))
  }

  private final class Tick(cb: Either[Throwable, Unit] => Unit)
    extends Runnable {
    def run() = cb(Callback.rightUnit)
  }

  private def setImmediate(r: Runnable): Unit =
    setImmediateRef(() =>
      try r.run()
      catch { case e: Throwable => e.printStackTrace() })

  private def setTimeout(delayMillis: Long, r: Runnable): js.Dynamic = {
    val lambda: js.Function = () =>
      try { r.run() }
      catch { case e: Throwable => e.printStackTrace() }

    js.Dynamic.global.setTimeout(lambda, delayMillis)
  }

  private def clearTimeout(task: js.Dynamic): js.Dynamic = {
    js.Dynamic.global.clearTimeout(task)
  }

  private final val setImmediateRef: js.Dynamic = {
    if (!js.isUndefined(js.Dynamic.global.setImmediate))
      js.Dynamic.global.setImmediate
    else
      js.Dynamic.global.setTimeout
  }
}
