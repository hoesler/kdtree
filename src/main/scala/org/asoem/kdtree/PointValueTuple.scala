package org.asoem.kdtree

trait PointValueTuple[+A] extends Product2[HyperPoint, A] {
  def point: HyperPoint
  def value : A

  override def _1 = point
  override def _2 = value
  override def canEqual(that: Any) = that.isInstanceOf[PointValueTuple[_]]

  override def toString = point.toString
}