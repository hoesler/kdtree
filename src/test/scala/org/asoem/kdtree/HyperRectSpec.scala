package org.asoem.kdtree

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class HyperRectSpec extends FlatSpec with Matchers {

  "Two HyperRect objects" can "be equal" in {
    val rect1 = HyperRect(
      HyperPoint(0, 0),
      HyperPoint(2, 2))
    val rect2 = HyperRect(
      HyperPoint(0, 0),
      HyperPoint(2, 2))

    assert(rect1 == rect2, "%s is not equal to %s".format(rect1, rect2))
  }

  "A HyperRect" can "intersect with a second" in {
    val rect1 = HyperRect(
      HyperPoint(0, 0),
      HyperPoint(2, 2))
    val rect2 = HyperRect(
      HyperPoint(1, 1),
      HyperPoint(3, 3))
    val rect3 = HyperRect(
      HyperPoint(3, 3),
      HyperPoint(4, 4))
    val rect4 = HyperRect(
      HyperPoint(0, 0),
      HyperPoint(4, 4))

    // test 3 cases of intersection:
    // partial intersection
    val expected = HyperRect(
      HyperPoint(1, 1),
      HyperPoint(2, 2))

    val intersections = List(
      // partial intersection
      (rect1 intersection rect2, expected),
      (rect2 intersection rect1, expected),
      // inclusion
      (rect4 intersection rect2, rect2),
      (rect2 intersection rect4, rect2),
      // disjoint
      (rect1 intersection rect3, null),
      (rect3 intersection rect1, null)
    )
    for (intersection <- intersections) {
      assert(intersection._1 == intersection._2, "HyperRects do not intersect: %s with %s yields %s. Expected was %s".format(rect1, rect2, intersection._1, intersection._2))
    }
  }

  it can "be split" in {
    val rect1 = HyperRect(
      HyperPoint(0, 0),
      HyperPoint(2, 2))
    val point = HyperPoint(1, 1)
    val splitPair = rect1.split(point, 0)

    assert(splitPair._1 == HyperRect(
      HyperPoint(0, 0),
      HyperPoint(1, 2)))
    assert(splitPair._2 == HyperRect(
      HyperPoint(1, 0),
      HyperPoint(2, 2)))
  }

  it can "contain points" in {
    val rect1 = HyperRect(
      HyperPoint(0, 0),
      HyperPoint(2, 2))
    val point = HyperPoint(1, 1)

    rect1.contains(point) should equal(true)
  }
}