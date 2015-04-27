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

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/**
 * A KD-Tree implementation
 *
 * Tree construction is done in less than O([k-1]n log n)
 * @param dim the dimension of the points in tree nodes
 * @param pointValueInput a sequence of point x value pairs to construct the tree from
 * @param forkJoinThreshold the threshold above which the construction will be done asynchronously
 * @tparam A the type of values the nodes will hold
 */
final class KDTree[A](val dim: Int, pointValueInput: Seq[Product2[HyperPoint, A]], forkJoinThreshold: Int)
                     (implicit xc: ExecutionContext = ExecutionContext.global)
  extends Tree[KDNode[A]] with Immutable {

  require(dim > 0, "Dimension must be > 0")
  require(pointValueInput != null, "Argument 'pointValueInput' must not be null")

  /** The size of this tree which is the number of nodes accessible via root
    *
    */
  val size = pointValueInput.length

  /** The root node
    *
    */
  val root = {

    val splitAxisFunction = if (dim == 2) (depth: Int) => {
      depth & 1
    } else (depth: Int) => {
      depth % dim
    }

    def createTree(sublist: Seq[Product2[HyperPoint, A]], depth: Int = 0): Option[KDNode[A]] = sublist.length match {
      case 0 => Option.empty

      case 1 =>
        val head: Product2[HyperPoint, A] = sublist.head
        require(head._1.dim == dim, "Dimension mismatch")
        Some(LeafNode(head._1, head._2, splitAxisFunction(depth)))

      case sublistLength =>

        val axis = splitAxisFunction(depth)
        val indexOfSplit = sublistLength / 2
        val (left, rightWithMedian) = sublist.sortWith(_._1(axis) < _._1(axis)).splitAt(indexOfSplit)
        val newDepth: Int = depth + 1

        val current: Product2[HyperPoint, A] = rightWithMedian.head
        require(current._1.dim == dim, "Dimension mismatch")

        val node: KDNode[A] = if (sublistLength > forkJoinThreshold) {
          val resultLeft = Future {
            createTree(left, newDepth)
          }
          val resultRight = Future {
            createTree(rightWithMedian.tail, newDepth)
          }

          Await.result(for {
            leftNode <- resultLeft
            rightNode <- resultRight
          } yield
            KDNode(
              current,
              axis,
              leftNode,
              rightNode
            )
            , Duration.Inf)
        } else {
          KDNode(
            current,
            axis,
            createTree(left, newDepth),
            createTree(rightWithMedian.tail, newDepth)
          )
        }

        Option(node)
    }

    createTree(pointValueInput)
  }

  /**
   * Filter all Nodes whose point is contained by the given Shape.
   *
   * The complexity of this algorithm is O(N)
   * @param shape the Shape that defines the search range
   * @return a list of NNResult objects
   */
  def filterRange(shape: Shape): List[KDNode[A]] = {
    require(shape != null, "shape must not be null")

    def search(nodeOption: Option[KDNode[A]]): List[KDNode[A]] = nodeOption match {
      case None => Nil

      case Some(node) =>
        val prefix =
          if (shape.contains(node.point))
            List(node)
          else Nil

        val postfix = if (!node.isLeaf) {
          search(node.leftChild) ::: search(node.rightChild) // TODO: Can this be optimized?
        }
        else
          Nil

        prefix ::: postfix
    }

    search(root)
  }

  /**
   * Filter all Nodes whose point is contained by the given HyperSphere.
   *
   * The complexity of this algorithm is O(log N)
   * @param sphere the HyperSphere that defines the search range
   * @return a list of NNResult objects
   */
  def filterRange(sphere: HyperSphere): List[NNResult[A]] = {
    require(sphere != null, "Argument 'range' must not be null")
    require(sphere.dim == dim, "Dimension of 'searchPoint' (%d) does not match dimension of this tree (%d).".format(sphere.dim, dim))

    def search(nodeOption: Option[KDNode[A]]): List[NNResult[A]] = nodeOption match {
      case None => Nil

      case Some(node) =>
        val dist = sphere.origin.distance(node.point)

        val prefix =
          if (dist <= sphere.radius)
            List(new NNResult(node, dist))
          else Nil

        val postfix = if (!node.isLeaf) {
          val distance = sphere.origin(node.splitDim) - node.point(node.splitDim)
          val closeChild = if (distance <= 0) node.leftChild else node.rightChild
          val farChild = if (distance <= 0) node.rightChild else node.leftChild

          // search in the HyperRect containing searchPoint
          val resultCloseChild = search(closeChild) // TODO: is the close child always contained by the sphere?

          val resultFarChild =
            if (math.abs(distance) <= sphere.radius)
              search(farChild)
            else
              Nil

          resultCloseChild ::: resultFarChild
        }
        else
          Nil

        prefix ::: postfix
    }

    search(root)
  }

  def findNeighbours(searchPoint: HyperPoint, k: Int = 1): List[NNResult[A]] = {

    require(searchPoint != null, "Argument 'searchPoint' must not be null")
    require(searchPoint.dim == dim, "Dimension of 'searchPoint' (%d) does not match dimension of this tree (%d).".format(searchPoint.dim, dim))
    require(k <= size, "k=%d must be <= size=%d".format(k, size))

    if (k == 0)
      return Nil

    var resultList = mutable.LinkedList[NNResult[A]]()
    var farNode = mutable.LinkedList[NNResult[A]]()

    def search(node: Option[KDNode[A]]) {
      if (node.isEmpty)
        return

      if (!node.get.isLeaf) {
        val translationInSplitDim = searchPoint(node.get.splitDim) - node.get.point(node.get.splitDim)
        val closeChild = if (translationInSplitDim <= 0) node.get.leftChild else node.get.rightChild
        val farChild = if (translationInSplitDim <= 0) node.get.rightChild else node.get.leftChild

        // search in the HyperRect containing searchPoint
        search(closeChild)

        // search in the HyperRect not containing searchPoint
        // if it intersects with searchRange
        if (farNode.isEmpty || new HyperSphere(searchPoint, farNode.elem.distance).contains(farChild.get.point))
          search(farChild)
      }

      val distanceToSearchPoint = searchPoint.distance(node.get.point)
      if (resultList.size < k) {
        val element = new NNResult(node.get, distanceToSearchPoint)
        val list = mutable.LinkedList[NNResult[A]](element)
        resultList = resultList.append(list)

        if (farNode.isEmpty || (farNode.elem compare element) < 0)
          farNode = list
      }
      else if (distanceToSearchPoint < farNode.elem.distance) {
        val element = new NNResult(node.get, distanceToSearchPoint)
        farNode.elem = element
      }
    }

    search(root)

    resultList.toList
  }
}

object KDTree {

  def defaultThreshold(size: Int) = math.max(size, 1000) / Runtime.getRuntime.availableProcessors

  def apply[A](dim: Int, pointValueTuples: Seq[Product2[HyperPoint, A]]): KDTree[A] = {
    new KDTree[A](dim, pointValueTuples, defaultThreshold(pointValueTuples.length))
  }

  /**
   * Creates a new KDTree from `pointValueTuples`.
   * Assumes that the dimension of the first point in `pointValueTuples` is the desired dimension of the tree.
   * Uses the DEFAULT_THRESHOLD for parallelization
   * @param pointValueTuples the point x value mappings the nodes will hold
   * @tparam A The type of the values the nodes will hold
   * @return a new KDTree of the assumed dimension
   */
  def apply[A](pointValueTuples: Seq[Product2[HyperPoint, A]]): KDTree[A] = {
    if (pointValueTuples.isEmpty)
      throw new IllegalArgumentException("Cannot make any assumptions about the dimension of the KDTree")
    else
      KDTree[A](pointValueTuples.head._1.dim, pointValueTuples)
  }

  def empty[A](dim: Int): KDTree[A] = new KDTree[A](dim, Nil, 0)
}
