package org.asoem.kdtree

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import scala._

@RunWith(classOf[JUnitRunner])
class KDTreeSpec extends FlatSpec with ShouldMatchers {
	
	val points : List[HyperPoint] = List(
		HyperPoint(2,3),
		HyperPoint(5,4),
		HyperPoint(9,6),
		HyperPoint(4,7),
		HyperPoint(8,1),
		HyperPoint(7,2))
	val tree = KDTree(points.map(e => (e, 0)))


  "A KDTree" should "contain corect number of keys after construction" in {
      assert(tree.size == points.size,
        tree.size + " != " + points.size)
  }

  it can "be searched for the nearest neighbours to a given point" in {
     val nodeList1 = tree.findNeighbours(HyperPoint(9.1, 6.1), k=1) map (e => e.point)
     val expected1 = List[HyperPoint](points(2))
		 assert( nodeList1.sameElements(expected1),
       "NNSearch reported " + nodeList1 + "; Expected was " + expected1 + " for tree\n" + tree )
  }

  it should "find an existing node by it's key" in {
    val expected = points(0)
    val resultList = tree.findNeighbours(expected, 1)
    assert(resultList.size == 1 && resultList.head.point == expected,
      "NNSearch reported " + resultList + "; Expected was " + expected + " for tree\n" + tree)
  }

  it should "handle duplicates" in {
    val points_with_dup = HyperPoint(7,2) :: points
    val tree_with_dup = KDTree(points_with_dup.map(e => (e, 0)))

    val resultList = tree_with_dup.filterRange(HyperPoint(7,2), 1.5)
    val resultListPoints = resultList.map(e => e.point)

    val expected = List[HyperPoint](points_with_dup(0), points_with_dup(6), points_with_dup(5))

		assert( resultListPoints.sameElements(expected),
      "NNSearch reported " + resultListPoints + "; Expected was " + expected + " for tree\n" + tree_with_dup )
  }
}
