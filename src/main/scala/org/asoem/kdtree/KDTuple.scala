package org.asoem.kdtree

object KDTuple {
  implicit def kdTupleToTuple[A](kdtuple : KDTuple[A]) = (kdtuple.point, kdtuple.value)
}

class KDTuple[A](val point: HyperPoint, val value : A) {

  override def toString: String = {
    point.toString
  }

  def dim = point.dim
}