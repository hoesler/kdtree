package org.asoem.kdtree

object KDTuple {
  implicit def kdTupleToTuple[A](kdtuple : KDTuple[A]) = (kdtuple.point, kdtuple.value)
}

case class KDTuple[A](point: HyperPoint, value : A) {

  override def toString: String = {
    point.toString
  }

  def dim = point.dim
}