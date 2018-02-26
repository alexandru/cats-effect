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

import org.scalactic.source.Position
import org.scalatest.{Assertion, AsyncFunSuite, Matchers}
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}
import scala.concurrent.duration._

/**
 * Tests doing real asynchrony for both the JVM and JS, by
 * means of ScalaTest's engine.
 */
class IOAsyncTests extends AsyncFunSuite with Matchers {
  implicit override def executionContext =
    ExecutionContext.global

  def testEffectOnRunAsync(source: IO[Int], expected: Try[Int])
    (implicit pos: Position): Future[Assertion] = {

    val effect = Promise[Int]()
    val attempt = Promise[Try[Int]]()
    effect.future.onComplete(attempt.success)

    val io = source.runAsync {
      case Right(a) => IO(effect.success(a))
      case Left(e) => IO(effect.failure(e))
    }

    for (_ <- io.unsafeToFuture(); v <- attempt.future) yield {
      v shouldEqual expected
    }
  }

  test("IO.pure#runAsync") {
    testEffectOnRunAsync(IO.pure(10), Success(10))
  }

  test("IO.apply#runAsync") {
    testEffectOnRunAsync(IO(10), Success(10))
  }

  test("IO.apply#shift#runAsync") {
    testEffectOnRunAsync(IO.shift.flatMap(_ => IO(10)), Success(10))
  }

  test("IO.raiseError#runAsync") {
    val dummy = new RuntimeException("dummy")
    testEffectOnRunAsync(IO.raiseError(dummy), Failure(dummy))
  }

  test("IO.raiseError#shift#runAsync") {
    val dummy = new RuntimeException("dummy")
    testEffectOnRunAsync(IO.shift.flatMap(_ => IO.raiseError(dummy)), Failure(dummy))
  }

  test("Timer[IO].shift") {
    for (_ <- Timer[IO].shift.unsafeToFuture()) yield {
      assert(1 == 1)
    }
  }

  test("Timer[IO].currentTimeMillis") {
    val time = System.currentTimeMillis()
    val io = Timer[IO].currentTimeMillis

    for (t2 <- io.unsafeToFuture()) yield {
      time should be > 0L
      time should be <= t2
    }
  }

  test("Timer[IO].sleep(10.ms)") {
    val io = Timer[IO].sleep(10.millis).map(_ => 10)

    for (r <- io.unsafeToFuture()) yield {
      r shouldBe 10
    }
  }

  test("Timer[IO].sleep(negative)") {
    val io = Timer[IO].sleep(-10.seconds).map(_ => 10)

    for (r <- io.unsafeToFuture()) yield {
      r shouldBe 10
    }
  }
}
