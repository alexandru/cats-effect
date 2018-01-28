/*
 * Copyright 2017 Typelevel
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

package cats.effect.internals

import java.util.concurrent.atomic.AtomicBoolean

import scala.util.Left
import cats.effect.internals.TrampolineEC.immediate

/**
 * Internal API — utilities for working with `IO.async` callbacks.
 */
private[effect] object Callback {
  type Type[-A] = Either[Throwable, A] => Unit

  /**
   * Builds a callback reference that throws any received
   * error immediately.
   */
  def report[A]: Type[A] =
    reportRef.asInstanceOf[Type[A]]

  private val reportRef = (r: Either[Throwable, _]) =>
    r match {
      case Left(e) => Logger.reportFailure(e)
      case _ => ()
    }

  /** Reusable `Right(())` reference. */
  final val rightUnit = Right(())

  /** Builds a callback with async execution. */
  def async[A](cb: Type[A]): Type[A] =
    async(null, cb)

  /**
   * Builds a callback with async execution.
   *
   * Also pops the `Connection` just before triggering
   * the underlying callback.
   */
  def async[A](conn: IOConnection, cb: Type[A]): Type[A] =
    value => immediate.execute(
      new Runnable {
        def run(): Unit = {
          if (conn ne null) conn.pop()
          cb(value)
        }
      })

  /**
   * Callback wrapper used in `IO.async` that:
   *
   *  1. guarantees (thread safe) idempotency
   *  2. triggers light (trampolined) async boundary for stack safety
   *  3. pops the given `Connection` (only if != null)
   *  4. logs extraneous errors after callback was already called once
   */
  def asyncIdempotent[A](conn: IOConnection, cb: Type[A]): Type[A] =
    new AsyncIdempotentCallback[A](conn, cb)

  /** Helpers async callbacks. */
  implicit final class Extensions[-A](val self: Type[A]) extends AnyVal {
    /**
     * Executes the source callback with a light (trampolined) async
     * boundary, meant to protect against stack overflows.
     */
    def async(value: Either[Throwable, A]): Unit =
      async(null, value)

    /**
     * Executes the source callback with a light (trampolined) async
     * boundary, meant to protect against stack overflows.
     *
     * Also pops the given `Connection` before calling the callback.
     */
    def async(conn: IOConnection, value: Either[Throwable, A]): Unit =
      immediate.execute(new Runnable {
        def run(): Unit = {
          if (conn ne null) conn.pop()
          self(value)
        }
      })
  }

  private final class AsyncIdempotentCallback[-A](
    conn: IOConnection,
    cb: Either[Throwable, A] => Unit)
    extends (Either[Throwable, A] => Unit) {

    private[this] val canCall = new AtomicBoolean(true)

    def apply(value: Either[Throwable, A]): Unit = {
      if (canCall.getAndSet(false)) {
        immediate.execute(new Runnable {
          def run(): Unit = {
            if (conn ne null) conn.pop()
            cb(value)
          }
        })
      } else value match {
        case Right(_) => ()
        case Left(e) =>
          Logger.reportFailure(e)
      }
    }
  }
}