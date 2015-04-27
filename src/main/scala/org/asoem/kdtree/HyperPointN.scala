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

final case class HyperPointN(coordinates: List[Double]) extends HyperPoint {
  require(coordinates != null)

  def this(point: HyperPoint) = this(point.coordinates)

  def this(coordinates: Double*) = this(coordinates.toList)

  def this(coordinates: Array[Double]) = this(coordinates.toList)

  def this(coordinates: Traversable[Double]) = this(coordinates.toList)

  def this(coordinates: java.lang.Iterable[Double]) = this(iterableAsScalaIterable(coordinates))

  def apply(idx: Int): Double = coordinates(idx)

  override def dim = coordinates.length

  def distance(that: HyperPoint): Double = {
    require(this.dim == that.dim)
    val coords1 = this.coordinates
    val coords2 = that.coordinates
    @tailrec
    def squareSum(sum: Double, index: Int): Double = index match {
      case 0 => sum
      case _ => val newIndex = index - 1; squareSum(sum + pow(coords1(newIndex) - coords2(newIndex), 2), newIndex)
    }
    sqrt(squareSum(0, dim))
  }

  def edit(dim: Int, value: Double): HyperPoint = HyperPoint(coordinates.updated(dim, value))

  def /(divisor: Double): HyperPoint = HyperPoint(coordinates.map(e => e / divisor))

  def -(that: HyperPoint) = HyperPoint((coordinates, that.coordinates).zipped.map(_ - _))

  def +(that: HyperPoint) = HyperPoint((coordinates, that.coordinates).zipped.map(_ + _))

  def -(c: Double) = HyperPoint(coordinates.map(_ - c))

  def +(c: Double) = HyperPoint(coordinates.map(_ + c))

}