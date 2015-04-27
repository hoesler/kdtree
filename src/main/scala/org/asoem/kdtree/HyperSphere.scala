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

final case class HyperSphere(origin: HyperPoint, radius: Double) extends Shape {

  require(origin != null)

  override def dim = origin.dim

  override def contains(point: HyperPoint): Boolean = {
    require(point != null)
    if (point.dim > this.dim)
      return false
    (origin distance point) < radius
  }

  def contains(rect: HyperRect): Boolean = {
    require(rect != null)
    contains(rect.min) && contains(rect.max)
  }

  def intersects(rect: HyperRect): Boolean = {
    require(rect != null)
    rect.intersects(this)
  }

  override val boundingBox = {
    var min: HyperPoint = origin - radius
    var max: HyperPoint = origin + radius
    HyperRect(min, max)
  }
}