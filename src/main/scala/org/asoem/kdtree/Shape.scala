package org.asoem.kdtree

trait Shape {
  def dim: Int

  def boundingBox: HyperRect

  def contains(point: HyperPoint): Boolean
}