package org.asoem.kdtree

import scala.math.abs

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