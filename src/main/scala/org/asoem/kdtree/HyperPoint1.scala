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

final case class HyperPoint1(x: Double) extends HyperPoint {

  def distance(that: HyperPoint): Double = {
    require(that.dim == 1)
    abs(x - that(0))
  }

  override def dim = 1

  def edit(dim: Int, value: Double) = dim match {
    case 0 => HyperPoint(x, value)
    case _ => throw new IndexOutOfBoundsException
  }

  def coordinates = List(x)

  def apply(idx: Int): Double = idx match {
    case 0 => x
    case _ => throw new IndexOutOfBoundsException
  }

  def /(divisor: Double): HyperPoint = HyperPoint(x / divisor)

  def -(c: Double) = HyperPoint(x - c)

  def +(c: Double) = HyperPoint(x + c)

  def -(that: HyperPoint): HyperPoint = {
    require(that.dim == 1)
    HyperPoint(x - that(0))
  }

  def +(that: HyperPoint): HyperPoint = {
    require(that.dim == 1)
    HyperPoint(x + that(0))
  }
}