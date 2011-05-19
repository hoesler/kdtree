package org.asoem.kdtree

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers

import scala.math.{sqrt, pow}

/**
 * User: christoph
 * Date: 16.02.11
 * Time: 11:19
 */
@RunWith(classOf[JUnitRunner])
class HyperPointSpec extends FlatSpec with ShouldMatchers {

  "A HyperPoint" should "correctly compute the distance to another point" in {
    val point1 = HyperPoint.at(2.5, 54.2, 76.3)
    val point2 = HyperPoint.at(54.2, 756.1, 4.1)

    val dist = sqrt(pow(point1(0) - point2(0), 2) + pow(point1(1) - point2(1), 2) + pow(point1(2) - point2(2), 2))
    point1.distance(point2) should equal(dist)
  }
}