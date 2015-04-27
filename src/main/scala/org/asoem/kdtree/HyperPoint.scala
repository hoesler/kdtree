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

trait HyperPoint extends Shape {

  def distance(that: HyperPoint): Double

  def edit(dim: Int, value: Double): HyperPoint

  def apply(idx: Int): Double

  def boundingBox = HyperRect(this, this)

  def coordinates: List[Double]

  override def toString: String = {
    "[" + coordinates.mkString(" ") + "]"
  }

  def /(divisor: Double): HyperPoint

  def -(c: Double): HyperPoint

  def +(c: Double): HyperPoint

  def -(that: HyperPoint): HyperPoint

  def +(that: HyperPoint): HyperPoint

  override def contains(point: HyperPoint): Boolean = {
    require(point != null)
    if (point.dim > this.dim)
      return false
    coordinates.slice(0, point.dim - 1).equals(point.coordinates)
  }
}

object HyperPoint {
  def zero(dim: Int): HyperPoint = fill(dim, () => 0)

  def min(dim: Int): HyperPoint = fill(dim, () => Double.MinValue)

  def max(dim: Int): HyperPoint = fill(dim, () => Double.MaxValue)

  def fill[A](dim: Int, f: () => Double): HyperPoint = dim match {
    case 1 => HyperPoint(f())
    case 2 => HyperPoint(f(), f())
    case _ => HyperPoint(List.fill(dim)(f()))
  }

  def apply(c1: Double): HyperPoint = {
    new HyperPoint1(c1)
  }

  def apply(c1: Double, c2: Double): HyperPoint = {
    new HyperPoint2(c1, c2)
  }

  def apply(coords: Array[Double]): HyperPoint = {
    apply(coords.toTraversable)
  }

  def apply(coords: Double*): HyperPoint = coords.length match {
    case 1 => new HyperPoint1(coords(0))
    case 2 => new HyperPoint2(coords(0), coords(1))
    case _ => new HyperPointN(coords)
  }

  def apply(coords: Traversable[Double]): HyperPoint = apply(coords.toArray: _*)
}