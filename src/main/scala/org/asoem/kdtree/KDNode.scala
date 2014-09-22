package org.asoem.kdtree

object KDNode {
  def apply[A](pointValueTuple: Product2[HyperPoint, A], splitDim: Int,
               leftChild: Option[KDNode[A]] = Option.empty,
               rightChild: Option[KDNode[A]] = Option.empty): KDNode[A] = {
    apply(pointValueTuple._1, pointValueTuple._2, splitDim, leftChild, rightChild)
  }

  def apply[A](point: HyperPoint, value: A, splitDim: Int,
               leftChild: Option[KDNode[A]], rightChild: Option[KDNode[A]]): KDNode[A] = (leftChild, rightChild) match {
    case (None, None) =>
      require(splitDim < point.dim)
      LeafNode(point, value, splitDim)
    case (Some(_), None) =>
      require(splitDim < point.dim && leftChild.get.dim == point.dim)
      LeftBranchNode(point, value, splitDim, leftChild.asInstanceOf[Some[KDNode[A]]])
    case (Some(_), Some(_)) =>
      require(splitDim < point.dim && leftChild.get.dim == point.dim && rightChild.get.dim == point.dim)
      FullBranchNode(point, value, splitDim, leftChild.asInstanceOf[Some[KDNode[A]]], rightChild.asInstanceOf[Some[KDNode[A]]])
    case _ => throw new AssertionError("A KD Tree has never a node with just a right child!")
  }
}

sealed abstract class KDNode[+A] extends PointValueTuple[A] {
  self =>

  def point: HyperPoint

  def value: A

  def splitDim: Int

  def leftChild: Option[KDNode[A]]

  @deprecated("Use leftChild.orNull instead", "1.2.6")
  def left = leftChild.orNull

  def rightChild: Option[KDNode[A]]

  @deprecated("Use rightChild.orNull instead", "1.2.6")
  def right = rightChild.orNull

  def isLeaf: Boolean

  def splitCoordinate = point(splitDim)

  def updatedLeft[B >: A](node: KDNode[B]) = KDNode[B](point, value, splitDim, Option(node), rightChild)

  def updatedRight[B >: A](node: KDNode[B]) = KDNode[B](point, value, splitDim, leftChild, Option(node))
}

final case class FullBranchNode[+A](point: HyperPoint, value: A,
                                    splitDim: Int, leftChild: Some[KDNode[A]],
                                    rightChild: Some[KDNode[A]]) extends KDNode[A] {
  override def isLeaf: Boolean = false
}

final case class LeftBranchNode[+A](point: HyperPoint, value: A, splitDim: Int,
                                    leftChild: Some[KDNode[A]]) extends KDNode[A] {
  override def isLeaf: Boolean = false

  override def rightChild: Option[KDNode[A]] = Option.empty
}

final case class LeafNode[+A](point: HyperPoint, value: A, splitDim: Int) extends KDNode[A] {
  override def leftChild: Option[KDNode[A]] = Option.empty

  override def rightChild: Option[KDNode[A]] = Option.empty

  override def isLeaf: Boolean = true
}
