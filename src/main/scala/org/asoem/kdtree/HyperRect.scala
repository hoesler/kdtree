package org.asoem.kdtree

import runtime.Int
import annotation.tailrec

case class HyperRect(min : HyperPoint, max : HyperPoint) extends HyperObject {
  require(min != null)
  require(max != null)
  require(min.dim == max.dim,
    "Dimensions of min and max do not match")
  require(min.coordinates.corresponds(max.coordinates)(_ <= _),
    "The coordinates of min must be smaller or equal to the corresponding coordinates of max")

  lazy val centerPoint =  min + (max - min) / 2
  def center() : HyperPoint = centerPoint

  override def dim = min.dim

  def contains(point : HyperPoint) : Boolean = {
    @tailrec
    def checkAxis(axis : Int) : Boolean = axis match {
      case 0 => true
      case x => {
        val dimToCheck = x - 1
        min(dimToCheck) <= point(dimToCheck) &&
        max(dimToCheck) >= point(dimToCheck) &&
        checkAxis(dimToCheck)
      }
    }
    checkAxis(dim)
  }

  def contains(that : HyperRect) : Boolean = {
    contains(that.min) && contains(that.max)
  }

  def contains(that : HyperSphere) : Boolean = {
    (min.coordinates, that.origin.coordinates).zipped.forall(_ <= _ - that.radius) &&
      (max.coordinates, that.origin.coordinates).zipped.forall(_ >= _ + that.radius)
  }

  def intersects(that : HyperSphere) : Boolean = {
    (this contains that.origin)  // TODO: implement
  }

  def intersects(that : HyperRect) : Boolean = {
    this.contains(that.min) || this.contains(that.max) ||
      that.contains(min) || that.contains(max)
  }

  def intersection(that : HyperRect) : HyperRect = {
    if (this intersects that) {
      val new_min = (min.coordinates, that.min.coordinates).zipped.map(math.max(_, _))
      val new_max = (max.coordinates, that.max.coordinates).zipped.map(math.min(_, _))
      return HyperRect(HyperPoint.at(new_min), HyperPoint.at(new_max))
    }

    null
  }

  def split(point : HyperPoint, splitDim : Int) : (HyperRect, HyperRect) = {
    if (this contains point) {
      val leftMax = max.edit(splitDim, point(splitDim))
      val rightMin = min.edit(splitDim, point(splitDim))
      val left = HyperRect(min, leftMax)
      val right = HyperRect(rightMin, max)
      return (left, right)
    }

    (this, null)
  }

  def boundingBox = this
}

object HyperRect {

  def max(dim : Int) : HyperRect = {
    HyperRect(HyperPoint.min(dim), HyperPoint.max(dim))
  }
}