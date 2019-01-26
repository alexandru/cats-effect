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
package internals

private[effect] object Platform {
  /**
   * Returns `true` if the underlying platform is the JVM,
   * or `false` if it's JavaScript or any other platform.
   *
   * Given that this is a constant, branches should be optimized
   * away by the compiler.
   */
  final val isJVM = false

  /**
   * Returns `true` if the underlying platform is JavaScript,
   * or `false` if it's the JVM or any other platform.
   *
   * Given that this is a constant, branches should be optimized
   * away by the compiler.
   */
  final val isJS = true
}
