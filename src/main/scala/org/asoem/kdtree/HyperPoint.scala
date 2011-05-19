package org.asoem.kdtree

import scala.math.{sqrt, pow}
import annotation.tailrec

trait HyperPoint extends HyperObject {

  def distance(that : HyperPoint) : Double

  def edit(dim : Int, value : Double) : HyperPoint

  def apply(idx : Int) : Double

  def boundingBox = HyperRect(this, this)

  def coordinates : List[Double]

  override def toString :String = {
    return "[" + coordinates.mkString(" ") + "]"
  }

  def /(divisor : Double) : HyperPoint
  def -(c : Double) : HyperPoint
  def +(c : Double) : HyperPoint

  def -(that : HyperPoint) : HyperPoint
  def +(that : HyperPoint) : HyperPoint
}

object HyperPoint {
  def zero(dim : Int) : HyperPoint = fill(dim, () => 0)
  def min(dim : Int) : HyperPoint = fill(dim, () => Double.MinValue)
  def max(dim : Int) : HyperPoint = fill(dim, () => Double.MaxValue)

  def fill[A](dim : Int, f : () => Double) : HyperPoint = dim match {
    case 2 => at(f(), f())
    case _ => at(List.fill(dim)(f()))
  }

  def at(c1 : Double, c2 : Double) : HyperPoint = {
    new HyperPoint2(c1, c2)
  }

  def at(coords : Double*) : HyperPoint = coords.length match {
    case 2 => new HyperPoint2(coords(0), coords(1))
    case _ => new HyperPointN(coords)
  }

  def at(coords : List[Double]) : HyperPoint = at(coords toArray : _*)
}