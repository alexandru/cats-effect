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

package cats
package effect

import simulacrum._
import cats.data.{EitherT, StateT, WriterT}
import cats.effect.internals.EffectFiber
import scala.annotation.implicitNotFound
import scala.util.Either

/**
 * A monad that can suspend side effects into the `F` context and
 * that supports lazy and potentially asynchronous evaluation.
 */
@typeclass
@implicitNotFound("""Cannot find implicit value for CEffect[${F}].
Building this implicit value might depend on having an implicit
s.c.ExecutionContext in scope, a Scheduler or some equivalent type.""")
trait CEffect[F[_]] extends CAsyncStart[F] with Effect[F] {
  /**
   * Evaluates `F[_]` with the ability to cancel it.
   *
   * The returned `IO[IO[Unit]]` is a suspended cancelable action that
   * can be used to cancel the running computation.
   *
   * Note that evaluating the returned `IO` value, along with
   * the boxed cancellable action are guaranteed to have immediate
   * (synchronous) execution so you can safely do this, even
   * on top of JavaScript (which has no ability to block threads):
   *
   * {{{
   *   val io = F.runCancelable(fa)(cb)
   *
   *   // For triggering asynchronous execution
   *   val cancel = io.unsafeRunSync
   *   // For cancellation
   *   cancel.unsafeRunSync
   * }}}
   */
  def runCancelable[A](fa: F[A])(cb: Either[Throwable, A] => IO[Unit]): IO[IO[Unit]]
}

private[effect] abstract class CEffectInstances {

  implicit def catsEitherTCEffect[F[_]: CEffect]: CEffect[EitherT[F, Throwable, ?]] =
    new CEffect[EitherT[F, Throwable, ?]] with CAsync.EitherTCAsync[F, Throwable]
      with Effect.EitherTEffect[F] {

      protected def F = CEffect[F]

      def runCancelable[A](fa: EitherT[F, Throwable, A])(cb: Either[Throwable, A] => IO[Unit]): IO[IO[Unit]] =
        F.runCancelable(fa.value)(cb.compose(_.right.flatMap(x => x)))
      def start[A](fa: EitherT[F, Throwable, A]) =
        EffectFiber.eitherT(fa)
    }

  implicit def catsStateTCEffect[F[_]: CEffect, S: Monoid]: CEffect[StateT[F, S, ?]] =
    new CEffect[StateT[F, S, ?]] with CAsync.StateTCAsync[F, S]
      with Effect.StateTEffect[F, S] {

      protected def F = CEffect[F]
      protected def S = Monoid[S]

      def runCancelable[A](fa: StateT[F, S, A])(cb: Either[Throwable, A] => IO[Unit]): IO[IO[Unit]] =
        F.runCancelable(fa.runA(Monoid[S].empty))(cb)
      def start[A](fa: StateT[F, S, A]) =
        EffectFiber.stateT(fa)
    }

  implicit def catsWriterTCEffect[F[_]: CEffect, L: Monoid]: CEffect[WriterT[F, L, ?]] =
    new CEffect[WriterT[F, L, ?]] with CAsync.WriterTCAsync[F, L]
      with Effect.WriterTEffect[F, L] {

      protected def F = CEffect[F]
      protected def L = Monoid[L]

      def runCancelable[A](fa: WriterT[F, L, A])(cb: Either[Throwable, A] => IO[Unit]): IO[IO[Unit]] =
        F.runCancelable(fa.run)(cb.compose(_.right.map(_._2)))
      def start[A](fa: WriterT[F, L, A]) =
        EffectFiber.writerT(fa)
    }
}

object CEffect extends CEffectInstances