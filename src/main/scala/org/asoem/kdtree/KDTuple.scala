package org.asoem.kdtree

object KDTuple {
  implicit def kdTupleToTuple[A](kdtuple : KDTuple[A]) = (kdtuple.point, kdtuple.value)
}

class KDTuple[A](val point: HyperPoint, val value : A) extends HyperObject {

  override def dim = point.dim
  override def boundingBox = point.boundingBox 

  override def toString = point.toString
}
