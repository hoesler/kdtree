package org.asoem.kdtree

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class HyperRectSpec extends FlatSpec with ShouldMatchers {
    "A HyperRect" can "intersect with a second" in {
		val rect1 = HyperRect(
				HyperPoint.at(0,0),
				HyperPoint.at(2,2))
		val rect2 = HyperRect(
				HyperPoint.at(1,1),
				HyperPoint.at(3,3))
		val rect3 = HyperRect(
				HyperPoint.at(3,3),
				HyperPoint.at(4,4))
		val rect4 = HyperRect(
				HyperPoint.at(0,0),
				HyperPoint.at(4,4))

		// test 3 cases of intersection:
		// partial intersection
		val expected = HyperRect(
				HyperPoint.at(1,1),
				HyperPoint.at(2,2))
		assert( (rect1 intersection rect2) == expected &&
				(rect2 intersection rect1) == expected )

		// inclusion
		assert( (rect4 intersection rect2) == rect2 &&
				(rect2 intersection rect4) == rect2 )

		// disjoint
		assert( (rect1 intersection rect3) == null &&
				(rect3 intersection rect1) == null)
	}

	it can "be split" in {
		val rect1 = HyperRect(
				HyperPoint.at(0,0),
				HyperPoint.at(2,2))
		val point = HyperPoint.at(1,1)
		val splitPair = rect1.split(point, 0)

		assert(splitPair._1 == HyperRect(
				HyperPoint.at(0,0),
				HyperPoint.at(1,2)))
		assert(splitPair._2 == HyperRect(
				HyperPoint.at(1,0),
				HyperPoint.at(2,2)))
	}

  it can "contain points" in {
    val rect1 = HyperRect(
				HyperPoint.at(0,0),
				HyperPoint.at(2,2))
		val point = HyperPoint.at(1,1)

    rect1.contains(point) should equal(true)
  }
}