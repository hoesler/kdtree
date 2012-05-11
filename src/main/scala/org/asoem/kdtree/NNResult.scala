package org.asoem.kdtree

class NNResult[A](override val point : HyperPoint, override val value: A, val distance : Double)
  extends KDTuple[A](point, value) with Ordered[NNResult[A]] {

  require(distance >= 0, "Distance must be > 0")

  def compare(that : NNResult[A]) = math.abs(this.distance) compare math.abs(that.distance)
}