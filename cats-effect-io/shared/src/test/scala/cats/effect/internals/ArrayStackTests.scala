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

package cats.effect.internals

import org.scalatest.{FunSuite, Matchers}

class ArrayStackTests extends FunSuite with Matchers with TestUtils {
  test("push and pop 8 items") {
    val stack = new ArrayStack[Int]()
    var times = 0

    while (times < 10) {
      assert(stack.isEmpty, "stack.isEmpty")
      for (i <- 0 until 8) stack.push(i)

      var list = List.empty[Int]
      while (!stack.isEmpty) {
        assert(!stack.isEmpty, "!stack.isEmpty")
        list = stack.pop() :: list
      }

      list shouldBe (0 until 8).toList
      stack.pop().asInstanceOf[AnyRef] shouldBe null
      stack.isEmpty shouldBe true

      times += 1
    }
  }

  test("push and pop 100 items") {
    val stack = new ArrayStack[Int]()
    var times = 0

    while (times < 10) {
      assert(stack.isEmpty, "stack.isEmpty")
      for (i <- 0 until 100) stack.push(i)

      var list = List.empty[Int]
      while (!stack.isEmpty) {
        assert(!stack.isEmpty, "!stack.isEmpty")
        list = stack.pop() :: list
      }

      list shouldBe (0 until 100).toList
      stack.pop().asInstanceOf[AnyRef] shouldBe null
      stack.isEmpty shouldBe true

      times += 1
    }
  }

  test("pushAll(stack)") {
    val stack = new ArrayStack[Int]()
    val stack2 = new ArrayStack[Int]()

    for (i <- 0 until 100) stack2.push(i)
    stack.pushAll(stack2)

    var list = List.empty[Int]
    while (!stack.isEmpty) {
      assert(!stack.isEmpty)
      list = stack.pop() :: list
    }

    list shouldBe (0 until 100).toList.reverse
    stack.pop().asInstanceOf[AnyRef] shouldBe null
    stack.isEmpty shouldBe true
    stack2.isEmpty shouldBe false
  }

  test("pushAll(iterable)") {
    val stack = new ArrayStack[Int]()
    val expected = (0 until 100).toList
    stack.pushAll(expected)

    var list = List.empty[Int]
    while (!stack.isEmpty) {
      assert(!stack.isEmpty)
      list = stack.pop() :: list
    }

    list shouldBe expected
    stack.pop().asInstanceOf[AnyRef] shouldBe null
    stack.isEmpty shouldBe true
  }

  test("iterator") {
    val stack = new ArrayStack[Int]()
    val expected = (0 until 100).toList
    for (i <- expected) stack.push(i)
    stack.iteratorReversed.toList shouldBe expected.reverse
  }
}
