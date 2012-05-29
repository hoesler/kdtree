package org.asoem.kdtree

object KDNode {

  def apply[A](pointValueTuple: Product2[HyperPoint, A], splitDim: Int, left: KDNode[A] = null, right: KDNode[A] = null) : KDNode[A] = {
    apply(pointValueTuple._1, pointValueTuple._2, splitDim, left, right)
  }

  def apply[A](point: HyperPoint, value : A, splitDim: Int, left: KDNode[A], right: KDNode[A]) : KDNode[A] = {
    apply(point, value, splitDim, (left, right))
  }

  def apply[A](point: HyperPoint, value : A, splitDim: Int, children : Product2[KDNode[A], KDNode[A]]) : KDNode[A] = children match {
    case (null, null) => new LeafNode(point, value, splitDim)
    case (_, null) => new HalfBranchNode(point, value, splitDim, children._1)
    case (null, _) => throw new AssertionError("A KD Tree has never a node with just a right child!")
    case (_, _) => new FullBranchNode(point, value, splitDim, children._1, children._2)
  }
}

trait KDNode[+A] extends PointValueTuple[A] {

  require(point.dim > splitDim)

  def point: HyperPoint
  def value: A
  def splitDim: Int

  def left: KDNode[A]
  def right: KDNode[A]
  def isLeaf: Boolean

  def splitCoordinate = point(splitDim)

  def updatedLeft[B>:A](node: KDNode[B]) = KDNode[B](point, value, splitDim, node, right)
  def updatedRight[B>:A](node: KDNode[B]) = KDNode[B](point, value, splitDim, left, node)
}

class FullBranchNode[+A](val point: HyperPoint, val value : A, val splitDim: Int, val left: KDNode[A], val right: KDNode[A]) extends KDNode[A] {
  def isLeaf = false
}

class HalfBranchNode[+A](val point: HyperPoint, val value : A, val splitDim: Int, val left: KDNode[A]) extends KDNode[A] {
  def right = null
  def isLeaf = false
}

class LeafNode[+A](val point: HyperPoint, val value : A, val splitDim: Int) extends KDNode[A] {
  def left = null
  def right = null
  def isLeaf = true
}
