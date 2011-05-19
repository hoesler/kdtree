package org.asoem.kdtree

case class NNResult[A](override val point : HyperPoint, override val value: A, distance : Double) extends KDTuple[A](point, value) with Ordered[NNResult[A]] {

  def compare(that : NNResult[A]) = this.distance compare that.distance
}