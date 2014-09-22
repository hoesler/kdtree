package org.asoem.kdtree

final class NNResult[+A](val node: KDNode[A], val distance: Double)
  extends PointValueTuple[A] with Ordered[NNResult[_]] {

  require(distance >= 0, "Distance must be > 0")

  override def value = node.value

  override def point = node.point

  override def compare(that: NNResult[_]) =
    math.abs(this.distance) compare math.abs(that.distance)
}