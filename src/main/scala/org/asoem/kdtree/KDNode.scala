package org.asoem.kdtree

case class KDNode[A](override val point: HyperPoint, override val value : A, splitDim: Int = -1, left: KDNode[A] = null, right: KDNode[A] = null) extends KDTuple[A](point, value) {

  def this(data : KDTuple[A], splitDim: Int, left: KDNode[A], right: KDNode[A]) = this(data.point, data.value, splitDim, left, right)

  def isLeaf: Boolean = {
    left == null && right == null
  }

  def splitCoord: Double = {
    point(splitDim)
  }

  def updatedLeft(node: KDNode[A]) : KDNode[A] = {
    KDNode(point, value, splitDim, node, right)
  }

  def updatedRight(node: KDNode[A]) : KDNode[A] = {
    KDNode(point, value, splitDim, left, node)
  }
}