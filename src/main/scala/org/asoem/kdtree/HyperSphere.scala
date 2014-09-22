package org.asoem.kdtree

final case class HyperSphere(origin: HyperPoint, radius: Double) extends Shape {

  require(origin != null)

  override def dim = origin.dim

  override def contains(point: HyperPoint): Boolean = {
    require(point != null)
    if (point.dim > this.dim)
      return false
    (origin distance point) < radius
  }

  def contains(rect: HyperRect): Boolean = {
    require(rect != null)
    contains(rect.min) && contains(rect.max)
  }

  def intersects(rect: HyperRect): Boolean = {
    require(rect != null)
    rect.intersects(this)
  }

  override val boundingBox = {
    var min: HyperPoint = origin - radius
    var max: HyperPoint = origin + radius
    HyperRect(min, max)
  }
}