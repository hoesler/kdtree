package org.asoem.kdtree

case class HyperSphere(origin : HyperPoint, radius : Double) extends HyperObject {
	
	require(origin != null)

  override def dim = origin.dim
	
	def contains(point : HyperPoint) : Boolean = {
		require(point != null)
		(origin distance point) < radius
	}
	
	def contains(rect : HyperRect) : Boolean = {
		require(rect != null)
		contains(rect.min) && contains(rect.max)
	}
	
	def intersects(rect : HyperRect) : Boolean = {
		require(rect != null)
		rect.intersects(this)
	}
	
	def boundingBox = {
		var min : HyperPoint = origin - radius;
		var max : HyperPoint = origin + radius;
		HyperRect(min, max)
	}
}