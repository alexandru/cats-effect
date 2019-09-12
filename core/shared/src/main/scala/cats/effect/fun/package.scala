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

package object fun {
  /**
   * Datatype for working doing working with "bifunctor IO",
   * as a very light-weight and effect-powered alternative
   * to `cats.data.EitherT`.
   *
   * This data type is a monad transformer that's defined
   * as an opaque type alias (newtype) for `F[+A]`, where
   * `F[_]` is a type that implements `MonadError[F, Throwable]`.
   *
   * Such types include effect types like [[IO]], but also simpler
   * types like `Try`. Because "constrained parametric polymorphism"
   * is used, type classes specify the constraints for individual methods, some
   * work just with `Applicative`, some need `MonadError`, some require [[Sync]],
   * etc. depending on how potent the used type is.
   *
   * ==Encoding==
   *
   * This is an opaque type definition — a "newtype" for  F[+A]`.
   * This encoding is inspired by:
   * [[https://github.com/alexknvl/newtypes alexknvl/newtypes]].
   *
   * In the future we'll be able to switch to real opaque types, see:
   * [[https://docs.scala-lang.org/sips/opaque-types.html SIP-35]]
   */
  type Bi[+F[_], +E, +A] <: Bi.Base with Bi.Tag
}
