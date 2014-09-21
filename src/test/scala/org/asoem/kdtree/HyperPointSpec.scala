package org.asoem.kdtree

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

import scala.math.{pow, sqrt}

@RunWith(classOf[JUnitRunner])
class HyperPointSpec extends FlatSpec with Matchers {

  "A HyperPointN" can "be created using Varargs" in {
    val point = HyperPoint(0.0)

    point shouldBe a[HyperPointN]
  }

  "A HyperPoint2" can "be created using Varargs" in {
    val point = HyperPoint(0.0, 0.0)

    point shouldBe a[HyperPoint2]
  }

  "A HyperPoint2" can "be created using an Array" in {
    val point = HyperPoint(Array(0.0, 0.0))

    point shouldBe a[HyperPoint2]
  }

  "A HyperPoint2" can "be created using a List" in {
    val point = HyperPoint(List(0.0, 0.0))

    point shouldBe a[HyperPoint2]
  }

  "Two HyperPoint objects" can "be equal" in {
    val point1 = HyperPoint(0, 0)
    val point2 = HyperPoint(0, 0)

    assert(point1 == point2, "%s is not equal to %s".format(point1, point2))
  }

  "A HyperPoint" should "correctly compute the distance to another point" in {
    val point1 = HyperPoint(2.5, 54.2, 76.3)
    val point2 = HyperPoint(54.2, 756.1, 4.1)

    val dist = sqrt(pow(point1(0) - point2(0), 2) + pow(point1(1) - point2(1), 2) + pow(point1(2) - point2(2), 2))
    point1.distance(point2) should equal(dist)
  }
}