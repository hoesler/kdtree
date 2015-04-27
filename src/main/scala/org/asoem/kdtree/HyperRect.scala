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

import scala.annotation.tailrec

final case class HyperRect(min: HyperPoint, max: HyperPoint) extends Shape {
  require(min != null)
  require(max != null)
  require(min.dim == max.dim,
    "Dimensions of min and max do not match")
  require(min.coordinates.corresponds(max.coordinates)(_ <= _),
    "The coordinates of min must be smaller or equal to the corresponding coordinates of max")

  lazy val centerPoint = min + (max - min) / 2

  def center(): HyperPoint = centerPoint

  override def dim = min.dim

  override def contains(point: HyperPoint): Boolean = {
    @tailrec
    def checkAxis(axis: Int): Boolean = axis match {
      case 0 => true
      case x =>
        val dimToCheck = x - 1
        min(dimToCheck) <= point(dimToCheck) &&
          max(dimToCheck) >= point(dimToCheck) &&
          checkAxis(dimToCheck)
    }
    checkAxis(dim)
  }

  def contains(that: HyperRect): Boolean = {
    contains(that.min) && contains(that.max)
  }

  def contains(that: HyperSphere): Boolean = {
    (min.coordinates, that.origin.coordinates).zipped.forall(_ <= _ - that.radius) &&
      (max.coordinates, that.origin.coordinates).zipped.forall(_ >= _ + that.radius)
  }

  def intersects(that: HyperSphere): Boolean = {
    this contains that.origin // TODO: implement
  }

  def intersects(that: HyperRect): Boolean = {
    this.contains(that.min) || this.contains(that.max) ||
      that.contains(min) || that.contains(max)
  }

  def intersection(that: HyperRect): HyperRect = {
    if (this intersects that) {
      val new_min = (min.coordinates, that.min.coordinates).zipped.map(math.max)
      val new_max = (max.coordinates, that.max.coordinates).zipped.map(math.min)
      return HyperRect(HyperPoint(new_min), HyperPoint(new_max))
    }

    null
  }

  def split(point: HyperPoint, splitDim: Int): (HyperRect, HyperRect) = {
    if (this contains point) {
      val leftMax = max.edit(splitDim, point(splitDim))
      val rightMin = min.edit(splitDim, point(splitDim))
      val left = HyperRect(min, leftMax)
      val right = HyperRect(rightMin, max)
      return (left, right)
    }

    (this, null)
  }

  override def boundingBox = this

  override def toString = "[%s,%s]".format(min, max)
}

object HyperRect {

  def max(dim: Int): HyperRect = {
    HyperRect(HyperPoint.min(dim), HyperPoint.max(dim))
  }
}