package org.asoem.kdtree

trait BinaryTreeNode[+T] extends TreeNode[KDNode[T]] {
  def leftChild: Option[KDNode[T]]

  def rightChild: Option[KDNode[T]]

  override def children = leftChild.orNull :: rightChild.orNull :: List()
}
