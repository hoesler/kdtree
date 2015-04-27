/*
 * Copyright 2015 The kdtree authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.asoem.kdtree

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class KDTreeSpec extends FlatSpec with Matchers {

  val points: List[HyperPoint] = List(
    HyperPoint(2, 3),
    HyperPoint(5, 4),
    HyperPoint(9, 6),
    HyperPoint(4, 7),
    HyperPoint(8, 1),
    HyperPoint(7, 2))

  "A KDTree" can "be constructed" in {
    val tree: KDTree[Int] = KDTree(points.map(e => (e, 0)))

    tree shouldNot be(null)
  }

  it should "contain corect number of keys after construction" in {
    val tree = KDTree(points.map(e => (e, 0)))

    assert(tree.size == points.size,
      tree.size + " != " + points.size)
  }

  it can "be searched for the nearest neighbours to a given point" in {
    val tree = KDTree(points.map(e => (e, 0)))
    val nodeList1 = tree.findNeighbours(HyperPoint(9.1, 6.1), k = 1) map (e => e.point)
    val expected1 = List[HyperPoint](points(2))
    assert(nodeList1.sameElements(expected1),
      "NNSearch reported " + nodeList1 + "; Expected was " + expected1 + " for tree\n" + tree)
  }

  it should "find an existing node by it's key" in {
    val tree = KDTree(points.map(e => (e, 0)))
    val expected = points(0)
    val resultList = tree.findNeighbours(expected, 1)
    assert(resultList.size == 1 && resultList.head.point == expected,
      "NNSearch reported " + resultList + "; Expected was " + expected + " for tree\n" + tree)
  }

  it should "handle duplicates" in {
    val tree = KDTree(points.map(e => (e, 0)))
    val points_with_dup = HyperPoint(7, 2) :: points
    val tree_with_dup = KDTree(points_with_dup.map(e => (e, 0)))

    val resultList = tree_with_dup.filterRange(HyperSphere(HyperPoint(7, 2), 1.5))
    val resultListPoints = resultList.map(e => e.point)

    val expected = List[HyperPoint](points_with_dup(0), points_with_dup(6), points_with_dup(5))

    assert(resultListPoints.sameElements(expected),
      "NNSearch reported " + resultListPoints + "; Expected was " + expected + " for tree\n" + tree_with_dup)
  }
}
