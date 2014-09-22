package org.asoem.kdtree

trait TreeNode[+A] {
  def isLeaf: Boolean

  def children: Iterable[A]
}
