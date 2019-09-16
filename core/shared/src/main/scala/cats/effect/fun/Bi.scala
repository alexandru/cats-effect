/*
 * Copyright (c) 2017-2019 The Typelevel Cats-effect Project Developers
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
package fun

import cats.data.EitherT
import cats.effect.ExitCase.{Canceled, Completed, Error}
import cats.implicits._
import cats.{Applicative, ApplicativeError, Functor, Monad, MonadError}

import scala.util.control.NoStackTrace

/**
 * Companion object for [[Bi]].
 *
 * See [[Bi.Methods]] for the documentation on its defined extension methods.
 */
object Bi extends BiInstances4 {
  // Part of the opaque type encoding
  private[fun] type Base
  private[fun] trait Tag extends Any

  /**
   * Extension methods for our opaque type.
   */
  implicit final class Methods[F[_], E, A](val self: Bi[F, E, A]) extends AnyVal {
    def underlying: F[A] =
      Bi.unwrap(self)

    def map[B](f: A => B)(implicit F: Functor[F]): Bi[F, E, B] =
      Bi.unsafeWrap(F.map(Bi.unwrap(self))(f))

    def flatMap[B](f: A => Bi[F, E, B])(implicit F: Monad[F]): Bi[F, E, B] =
      Bi.unsafeWrap(F.flatMap(Bi.unwrap(self))(f.asInstanceOf[A => F[B]]))

    def attempt(implicit F: MonadError[F, Throwable]): Bi[F, Nothing, Either[E, A]] = {
      Bi.unsafeWrap(
        Bi.unwrap(self).attempt.flatMap {
          case right @ Right(_) =>
            F.pure(right.asInstanceOf[Either[E, A]])
          case Left(WrappedErrorException(e)) =>
            F.pure(Left(e.asInstanceOf[E]) : Either[E, A])
          case Left(e) =>
            F.raiseError(e) : F[Either[E, A]]
        })
    }

    def errorWiden[EE >: E]: Bi[F, EE, A] = self

    def errorMap[C](f: E => C)(implicit F: ApplicativeError[F, Throwable]): Bi[F, C, A] =
      Bi.unsafeWrap(
        F.recoverWith(Bi.unwrap(self)) {
          case WrappedErrorException(e) =>
            F.raiseError(WrappedErrorException(f(e.asInstanceOf[E])))
        })

    def handleError(f: E => A)(implicit F: ApplicativeError[F, Throwable]): Bi[F, Nothing, A] =
      Bi.unsafeWrap(
        F.recover(Bi.unwrap(self)) {
          case WrappedErrorException(e) =>
            f(e.asInstanceOf[E])
        })

    def handleErrorWith(f: E => Bi[F, E, A])(implicit F: ApplicativeError[F, Throwable]): Bi[F, E, A] =
      Bi.unsafeWrap(
        F.recoverWith(Bi.unwrap(self)) {
          case WrappedErrorException(e) =>
            Bi.unwrap(f(e.asInstanceOf[E]))
        })

    def bracket[B](use: A => Bi[F, E, B])(release: A => Bi[F, Nothing, Unit])
      (implicit F: Bracket[F, Throwable]): Bi[F, E, B] = {

      Bi.unsafeWrap(F.bracket(Bi.unwrap(self))(use.asInstanceOf[A => F[B]])(release.asInstanceOf[A => F[Unit]]))
    }

    def bracketCase[B](use: A => Bi[F, E, B])(release: (A, ExitCase[E]) => Bi[F, Nothing, Unit])
      (implicit F: Bracket[F, Throwable]): Bi[F, E, B] = {

      Bi.unsafeWrap(F.bracketCase(Bi.unwrap(self))(use.asInstanceOf[A => F[B]]) { (a, ec) =>
        ec match {
          case Completed =>
            Bi.unwrap(release(a, Completed))
          case Error(WrappedErrorException(e)) =>
            Bi.unwrap(release(a, Error(e.asInstanceOf[E])))
          case Error(_) =>
            Bi.unwrap(release(a, Completed))
          case Canceled =>
            Bi.unwrap(release(a, Canceled))
        }
      })
    }

    def rethrowT(implicit F: ApplicativeError[F, Throwable], ev: E <:< Throwable): F[A] =
      rethrowViaT(identity[E])

    def rethrowViaT(f: E => Throwable)(implicit F: ApplicativeError[F, Throwable]): F[A] =
      F.recoverWith(Bi.unwrap(self)) {
        case WrappedErrorException(e) =>
          F.raiseError(f(e.asInstanceOf[E]))
      }

    def toEitherF(implicit F: ApplicativeError[F, Throwable]): F[Either[E, A]] =
      F.recover(F.map(Bi.unwrap(self))(a => Right(a) : Either[E, A])) {
        case WrappedErrorException(e) =>
          Left(e.asInstanceOf[E])
      }

    def toEitherT(implicit F: ApplicativeError[F, Throwable]): EitherT[F, E, A] =
      EitherT(toEitherF)

    def fold[B](fe: E => B, fa: A => B)(implicit F: ApplicativeError[F, Throwable]): F[B] =
      F.recover(F.map(Bi.unwrap(self))(fa)) {
        case WrappedErrorException(e) =>
          fe(e.asInstanceOf[E])
      }

    def foldF[B](fe: E => F[B], fa: A => F[B])(implicit F: MonadError[F, Throwable]): F[B] =
      F.recoverWith(F.flatMap(Bi.unwrap(self))(fa)) {
        case WrappedErrorException(e) =>
          fe(e.asInstanceOf[E])
      }
  }

  def apply[F[_]]: Apply[F] = new Apply[F]

  def pure[F[_], A](a: A)(implicit F: Applicative[F]): Bi[F, Nothing, A] =
    unsafeWrap(F.pure(a))

  def raiseError[F[_], E](e: E)(implicit F: ApplicativeError[F, Throwable]): Bi[F, E, Nothing] =
    unsafeWrap(F.raiseError(new WrappedErrorException(e)))

  def liftF[F[_], A](fa: F[A])(implicit F: ApplicativeError[F, Throwable]): Bi[F, Throwable, A] =
    unsafeWrap(F.handleErrorWith(fa)(e => F.raiseError(WrappedErrorException(e))))

  def delay[F[_], A](thunk: => A)(implicit F: Sync[F]): Bi[F, Throwable, A] =
    liftF(F.delay(thunk))

  def suspend[F[_], A](thunk: => Bi[F, Throwable, A])(implicit F: Sync[F]): Bi[F, Throwable, A] =
    liftF(F.suspend(unwrap(thunk)))

  def tailRecM[F[_], E, A, B](a: A)(f: A => Bi[F, E, Either[A, B]])(implicit F: Monad[F]): Bi[F, E, B] =
    Bi.unsafeWrap(F.tailRecM(a)(f.asInstanceOf[A => F[Either[A, B]]]))

  def async[F[_], A](k: (Either[Throwable, A] => Unit) => Unit)(implicit F: Async[F]): Bi[F, Throwable, A] =
    Bi.unsafeWrap(F.async[A] { cb =>
      k {
        case Left(e) => cb(Left(WrappedErrorException(e)))
        case other => cb(other)
      }
    })

  def asyncF[F[_], A](k: (Either[Throwable, A] => Unit) => Bi[F, Throwable, Unit])(implicit F: Async[F]): Bi[F, Throwable, A] =
    Bi.unsafeWrap(F.asyncF[A] { cb =>
      Bi.unwrap(k {
        case Left(e) => cb(Left(WrappedErrorException(e)))
        case other => cb(other)
      })
    })

  @inline def unsafeWrap[F[_], E, A](fa: F[A]): Bi[F, E, A] =
    fa.asInstanceOf[Bi[F, E, A]]

  @inline def unwrap[F[_], E, A](fea: Bi[F, E, A]): F[A] =
    fea.asInstanceOf[F[A]]

  /**
   * For currying the type parameters.
   */
  class Apply[F[_]](val dummy: Unit = ()) extends AnyVal {
    /** See [[Bi.pure]]. */
    def pure[A](a: A)(implicit F: Applicative[F]): Bi[F, Nothing, A] =
      Bi.pure(a)(F)

    /** See [[Bi.raiseError]]. */
    def raiseError[E](e: E)(implicit F: MonadError[F, Throwable]): Bi[F, E, Nothing] =
      Bi.raiseError(e)(F)

    /** See [[Bi.liftF]]. */
    def liftF[A](fa: F[A])(implicit F: ApplicativeError[F, Throwable]): Bi[F, Throwable, A] =
      Bi.liftF(fa)(F)

    /** See [[Bi.delay]]. */
    def delay[A](a: => A)(implicit F: Sync[F]): Bi[F, Throwable, A] =
      Bi.delay(a)(F)

    /** See [[Bi.suspend]]. */
    def suspend[A](fa: => Bi[F, Throwable, A])(implicit F: Sync[F]): Bi[F, Throwable, A] =
      Bi.suspend(fa)(F)

    /** See [[Bi.tailRecM]]. */
    def tailRecM[E, A, B](a: A)(f: A => Bi[F, E, Either[A, B]])(implicit F: Monad[F]): Bi[F, E, B] =
      Bi.tailRecM(a)(f)

    /** See [[Bi.async]]. */
    def async[A](k: (Either[Throwable, A] => Unit) => Unit)(implicit F: Async[F]): Bi[F, Throwable, A] =
      Bi.async(k)(F)

    /** See [[Bi.asyncF]]. */
    def asyncF[A](k: (Either[Throwable, A] => Unit) => Bi[F, Throwable, Unit])(implicit F: Async[F]): Bi[F, Throwable, A] =
      Bi.asyncF(k)(F)
  }

  /**
   * Internal — used for raising errors.
   */
  private final class WrappedErrorException[+E](e: E)
    extends RuntimeException with NoStackTrace {

    val error = Some(e)
    override def toString: String = s"WrappedErrorException($e)"
  }

  private object WrappedErrorException {
    @inline def apply[E](e: E): WrappedErrorException[E] =
      new WrappedErrorException[E](e)

    def unapply(arg: WrappedErrorException[Any]): Option[Any] =
      arg.error
  }
}

private[fun] abstract class BiInstances4 extends BiInstances3 {
  /**
   * Type class instance for [[Async]].
   */
  implicit final def biAsync[F[_]](implicit F: Async[F]): Async[Bi[F, Throwable, *]] =
    new BiAsync[F] { override val ev = F }

  protected trait BiAsync[F[_]]
    extends Async[Bi[F, Throwable, *]]  with BiSync[F] {

    implicit protected def ev: Async[F]

    override final def async[A](k: (Either[Throwable, A] => Unit) => Unit): Bi[F, Throwable, A] =
      Bi.async(k)(ev)
    override final def asyncF[A](k: (Either[Throwable, A] => Unit) => Bi[F, Throwable, Unit]): Bi[F, Throwable, A] =
      Bi.asyncF(k)(ev)
    override final def liftIO[A](ioa: IO[A]): Bi[F, Throwable, A] =
      Bi.liftF(ev.liftIO(ioa))(ev)
    override final def never[A]: Bi[F, Throwable, A] =
      Bi.unsafeWrap(ev.never)
  }
}

private[fun] abstract class BiInstances3 extends BiInstances2 {
  /**
   * Type class instance for [[Sync]].
   */
  implicit final def biSync[F[_]](implicit F: Sync[F]): Sync[Bi[F, Throwable, *]] =
    new BiSync[F] { override val ev = F }

  protected trait BiSync[F[_]]
    extends Sync[Bi[F, Throwable, *]]  with BiBracket[F, Throwable] {

    implicit protected def ev: Sync[F]

    override final def delay[A](thunk: => A): Bi[F, Throwable, A] =
      Bi.delay(thunk)(ev)
    override final def suspend[A](thunk: => Bi[F, Throwable, A]): Bi[F, Throwable, A] =
      Bi.suspend(thunk)(ev)
  }
}

private[fun] abstract class BiInstances2 extends BiInstances1 {
  /**
   * Type class instance for [[Bracket]].
   */
  implicit final def biBracket[F[_], E](implicit F: Bracket[F, Throwable]): Bracket[Bi[F, E, *], E] =
    new BiBracket[F, E] { override val ev = F }

  protected trait BiBracket[F[_], E]
    extends Bracket[Bi[F, E, *], E]  with BiMonadError[F, E] {

    implicit protected def ev: Bracket[F, Throwable]

    override final def bracketCase[A, B](acquire: Bi[F, E, A])(use: A => Bi[F, E, B])(release: (A, ExitCase[E]) => Bi[F, E, Unit]): Bi[F, E, B] =
      acquire.bracketCase(use)(release.asInstanceOf[(A, ExitCase[E]) => Bi[F, Nothing, Unit]])
  }
}

private[fun] abstract class BiInstances1 extends BiInstances0 {
  /**
   * Type class instance for `cats.MonadError`.
   */
  implicit final def biMonadError[F[_], E](implicit F: MonadError[F, Throwable]): MonadError[Bi[F, E, *], E] =
    new BiMonadError[F, E] { override val ev = F }

  protected trait BiMonadError[F[_], E]
    extends MonadError[Bi[F, E, *], E] with BiApplicativeError[F, E] {

    implicit protected def ev: MonadError[F, Throwable]

    override final def flatMap[A, B](fa: Bi[F, E, A])(f: A => Bi[F, E, B]): Bi[F, E, B] =
      fa.flatMap(f)
    override final def tailRecM[A, B](a: A)(f: A => Bi[F, E, Either[A, B]]): Bi[F, E, B] =
      Bi.tailRecM(a)(f)(ev)
  }
}

private[fun] abstract class BiInstances0 {
  /**
   * Type class instance for `cats.ApplicativeError`.
   */
  implicit final def biApplicativeError[F[_], E](implicit F: ApplicativeError[F, Throwable]): ApplicativeError[Bi[F, E, *], E] =
    new BiApplicativeError[F, E] { override val ev = F }

  protected trait BiApplicativeError[F[_], E]
    extends ApplicativeError[Bi[F, E, *], E] {

    implicit protected def ev: ApplicativeError[F, Throwable]

    override final def raiseError[A](e: E): Bi[F, E, A] =
      Bi.raiseError(e)(ev)
    override final def handleErrorWith[A](fa: Bi[F, E, A])(f: E => Bi[F, E, A]): Bi[F, E, A] =
      fa.handleErrorWith(f)
    override final def handleError[A](fa: Bi[F, E, A])(f: E => A): Bi[F, E, A] =
      fa.handleError(f)
    override final def pure[A](x: A): Bi[F, E, A] =
      Bi.pure(x)(ev)
    override final def ap[A, B](ff: Bi[F, E, A => B])(fa: Bi[F, E, A]): Bi[F, E, B] =
      Bi.unsafeWrap(ev.ap(Bi.unwrap(ff))(Bi.unwrap(fa)))
    override final def map[A, B](fa: Bi[F, E, A])(f: A => B): Bi[F, E, B] =
      fa.map(f)
  }
}
