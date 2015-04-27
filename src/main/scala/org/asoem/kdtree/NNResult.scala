/*
 * Copyright 2015 The kdtree authors
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

package org.asoem.kdtree

final class NNResult[+A](val node: KDNode[A], val distance: Double)
  extends PointValueTuple[A] with Ordered[NNResult[_]] {

  require(distance >= 0, "Distance must be > 0")

  override def value = node.value

  override def point = node.point

  override def compare(that: NNResult[_]) =
    math.abs(this.distance) compare math.abs(that.distance)
}