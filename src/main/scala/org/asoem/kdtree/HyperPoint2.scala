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

import scala.math.{pow, sqrt}

final case class HyperPoint2(x: Double, y: Double) extends HyperPoint {

  def distance(that: HyperPoint): Double = {
    require(that.dim == 2)
    sqrt(pow(this(0) - that(0), 2) + pow(this(1) - that(1), 2))
  }

  override def dim = 2

  def edit(dim: Int, value: Double) = dim match {
    case 0 => HyperPoint(value, this(1))
    case 1 => HyperPoint(this(0), value)
    case _ => throw new IndexOutOfBoundsException
  }

  def coordinates = List(x, y)

  def apply(idx: Int): Double = idx match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException
  }

  def /(divisor: Double): HyperPoint = HyperPoint(this(0) / divisor, this(1) / divisor)

  def -(c: Double) = HyperPoint(this(0) - c, this(1) - c)

  def +(c: Double) = HyperPoint(this(0) + c, this(1) + c)

  def -(that: HyperPoint): HyperPoint = {
    require(that.dim == 2)
    HyperPoint(this(0) - that(0), this(1) - that(1))
  }

  def +(that: HyperPoint): HyperPoint = {
    require(that.dim == 2)
    HyperPoint(this(0) + that(0), this(1) + that(1))
  }
}