package org.asoem.kdtree;

import scala.math.{sqrt, pow}

case class HyperPoint2(c1 : Double, c2 : Double) extends HyperPoint {

  def distance(that : HyperPoint) : Double = {
    require(that.dim == 2)
    sqrt(pow(this(0) - that(0), 2) + pow(this(1) - that(1), 2))
  }

  def dim = 2

  def edit(dim : Int, value : Double) = dim match {
    case 0 => HyperPoint.at(value, this(1))
    case 1 => HyperPoint.at(this(0), value)
    case _ => throw new IndexOutOfBoundsException
  }

  def coordinates = List(c1, c2)

  def apply(idx : Int) : Double = idx match {
    case 0 => c1
    case 1 => c2
    case _ => throw new IndexOutOfBoundsException
  }

  def /(divisor : Double) : HyperPoint = HyperPoint.at(this(0) / divisor, this(1) / divisor)

  def -(c : Double) = HyperPoint.at(this(0) - c, this(1) - c)
  def +(c : Double) = HyperPoint.at(this(0) + c, this(1) + c)

  def -(that : HyperPoint) : HyperPoint = {
    require(that.dim == 2)
    HyperPoint.at(this(0) - that(0), this(1) - that(1))
  }
  def +(that : HyperPoint) : HyperPoint = {
    require(that.dim == 2);
    HyperPoint.at(this(0) + that(0), this(1) + that(1))
  }
}