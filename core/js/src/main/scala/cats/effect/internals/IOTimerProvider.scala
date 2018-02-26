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

import cats.effect.internals.TimerImplementation.AsyncTimer

private[effect] abstract class IOTimerProvider {
  /**
   * Returns a [[Timer]] instance for [[IO]], built from a
   * Scala `ExecutionContext` and a Java `ScheduledExecutorService`.
   *
   * This timer is built implicitly, if the required execution context
   * and scheduler are available in scope. The scheduler is optional,
   * if one isn't available, the implementation falls back on the
   * builder requiring just the context.
   *
   * N.B. this is the JVM-specific version. On top of JavaScript
   * the implementation needs no `ExecutionContext` or
   * `ScheduledExecutorService` available in scope.
   */
  implicit lazy val timer: Timer[IO] =
    new AsyncTimer()
}
