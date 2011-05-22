package org.asoem.kdtree;

import scala.collection.JavaConversions.iterableAsScalaIterable
import annotation.tailrec
import scala.math.{sqrt, pow}

case class HyperPointN(coordinates : List[Double]) extends HyperPoint {
  require(coordinates != null)

  def this(point : HyperPoint) = this(point.coordinates)
  def this(coordinates : Double*) = this(coordinates toList)
  def this(coordinates : Traversable[Double]) = this(coordinates toList)
  def this(coordinates : java.lang.Iterable[Double]) = this(iterableAsScalaIterable(coordinates))

  def apply(idx : Int) : Double = coordinates(idx)

  override def dim = coordinates.length

  def distance(that : HyperPoint) : Double = {
    require(this.dim == that.dim)
    val coords1 = this.coordinates
    val coords2 = that.coordinates
    @tailrec
    def squareSum(sum : Double, index : Int) : Double = index match {
      case 0  => sum
      case _  => val newIndex = index-1; squareSum(sum + pow(coords1(newIndex) - coords2(newIndex), 2), newIndex)
    }
    sqrt(squareSum(0, dim))
  }

  def edit(dim : Int, value : Double) : HyperPoint = HyperPoint(coordinates.updated(dim, value))

  def /(divisor : Double) : HyperPoint = HyperPoint(coordinates.map(e => e / divisor))

  def -(that : HyperPoint) = HyperPoint((coordinates, that.coordinates).zipped.map(_ - _))

  def +(that : HyperPoint) = HyperPoint((coordinates, that.coordinates).zipped.map(_ + _))

  def -(c : Double) = HyperPoint(coordinates.map(_-c))

  def +(c : Double) = HyperPoint(coordinates.map(_+c))

}