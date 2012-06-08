package org.asoem.kdtree

import scala.actors.Futures._
import scala.collection.mutable.LinkedList
import scala._

class KDTree[+A](val dim : Int, pointValueInput : Seq[Product2[HyperPoint, A]], forkJoinThreshold : Int) extends HyperObject {

  require(pointValueInput != null, "Argument 'pointValueTuples' must not be null")

  /** The size of this tree which is the number of nodes accessible via root
   *
   */
  val size = pointValueInput.length

  /** The root node
   *
   */
  val root = {

    val splitAxisFunction = if (dim == 2) (depth:Int) => {depth & 1} else (depth:Int) => {depth % dim}

    def checkDim(node : KDNode[A]) : KDNode[A] = {
      if (node.point.dim != this.dim)
        throw new IllegalArgumentException("Point has differing dimension: " + node.point.dim + " != " + this.dim)
      else
        node
    }

    def append(sublist : Seq[Product2[HyperPoint, A]], depth : Int = 0) : KDNode[A] = sublist.length match {
      case 0 => null

      case 1 =>
        checkDim(new LeafNode(sublist.head._1, sublist.head._2, splitAxisFunction(depth)))

      case sublistLength =>

        val axis = splitAxisFunction(depth)
        val indexOfSplit = sublistLength / 2
        val (left, rightWithMedian) = sublist.sortWith(_._1(axis) < _._1(axis)).splitAt(indexOfSplit)

        val newDepth: Int = depth + 1
        val appendLeft = () => append(left, newDepth)
        val appendRight = () => append(rightWithMedian.tail, newDepth)

        val doFork = (sublistLength > forkJoinThreshold)
        val resultLeft = if (doFork) future{appendLeft()} else appendLeft
        val resultRight = if (doFork) future{appendRight()} else appendRight

        checkDim(
          KDNode(
            rightWithMedian.head,
            axis,
            resultLeft(),
            resultRight()
          )
        )
    }

    if (dim == 0) null else append(pointValueInput)
  }

  def filterRange(origin : HyperPoint, range : Double) : List[NNResult[A]] =
    filterRange(new HyperSphere(origin, range))

  def filterRange(range : HyperSphere) : List[NNResult[A]] = {
    require(range != null, "Argument 'range' must not be null")
    require(range.dim == dim, "Dimension of 'searchPoint' (%d) does not match dimension of this tree (%d).".format(range.dim, dim))

    def search(node : KDNode[A]) : List[NNResult[A]] = node match {
      case null => Nil

      case x:LeafNode[_] =>
        val dist = range.origin.distance(node.point)
        if (dist <= range.radius)
          List(new NNResult(node, dist))
        else Nil

      case _    =>
        val dist = range.origin.distance(node.point)

        val prefix =
          if (dist <= range.radius)
            List(new NNResult(node, dist))
          else Nil

        val postfix = if ( ! node.isLeaf ) {
          val distance = range.origin(node.splitDim) - node.point(node.splitDim)
          val closeChild = if (distance <= 0) node.left else node.right
          val farChild = if (distance <= 0) node.right else node.left

          // search in the HyperRect containing searchPoint
          val resultCloseChild = search(closeChild)

          val resultFarChild =
            if (math.abs(distance) <= range.radius)
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

  def findNeighbours(searchPoint : HyperPoint, k: Int = 1) : List[NNResult[A]] = {

    require(searchPoint != null, "Argument 'searchPoint' must not be null")
    require(searchPoint.dim == dim, "Dimension of 'searchPoint' (%d) does not match dimension of this tree (%d).".format(searchPoint.dim, dim))
    require(k <= size, "k=%d must be <= size=%d".format(k, size))

    if (k == 0)
      return Nil

    var resultList = LinkedList[NNResult[A]]()
    var farNode = LinkedList[NNResult[A]]()

    def search(node : KDNode[A]) {
      if (node == null)
        return

      if ( ! node.isLeaf ) {
        val translationInSplitDim = searchPoint(node.splitDim) - node.point(node.splitDim)
        val closeChild = if (translationInSplitDim <= 0) node.left else node.right
        val farChild = if (translationInSplitDim <= 0) node.right else node.left

        // search in the HyperRect containing searchPoint
        search(closeChild)

        // search in the HyperRect not containing searchPoint
        // if it intersects with searchRange
        if (farNode.isEmpty || new HyperSphere(searchPoint, farNode.elem.distance).contains(farChild.point))
          search(farChild)
      }

      val distanceToSearchPoint = searchPoint.distance(node.point)
      if (resultList.size < k) {
        val element = new NNResult(node, distanceToSearchPoint)
        val list = LinkedList[NNResult[A]](element)
        resultList = resultList.append(list)

        if (farNode.isEmpty || (farNode.elem compare element) < 0)
          farNode = list
      }
      else if (distanceToSearchPoint < farNode.elem.distance) {
        val element = new NNResult(node, distanceToSearchPoint)
        farNode.elem = element
      }
    }

    search(root)

    resultList.toList
  }

  override lazy val boundingBox = {
    var min = HyperPoint.zero(dim)
    var max = HyperPoint.zero(dim)
    for (i <- 0 to dim-1) {
      val sorted = pointValueInput.sortWith((e1, e2) => e1._1(i) < e2._1(i))
      min = min.edit(i, math.min(min(i), sorted.head._1(i)))
      max = max.edit(i, math.max(max(i), sorted.last._1(i)))
    }
    HyperRect(min, max)
  }
}

object KDTree {

  def apply[A](dim: Int, pointValueTuples : Seq[Product2[HyperPoint, A]]) : KDTree[A] = {
    new KDTree[A](dim, pointValueTuples, math.max(pointValueTuples.size, 1000) / Runtime.getRuntime.availableProcessors)
  }

  /**
   * Creates a new KDTree from `pointValueTuples`.
   * Assumes that the dimension of the first point in `pointValueTuples` is the desired dimension of the tree.
   * Uses the DEFAULT_THRESHOLD for parallelization
   * @param pointValueTuples the point x value mappings the nodes will hold
   * @tparam A The type of the values the nodes will hold
   * @return a new KDTree of the assumed dimension
   */
  def apply[A](pointValueTuples : Seq[Product2[HyperPoint, A]]) : KDTree[A] = {
    if (pointValueTuples.isEmpty)
      throw new IllegalArgumentException("Cannot make any assumptions about the dimension of the KDTree")
    else
      KDTree[A](pointValueTuples.head._1.dim ,pointValueTuples)
  }

  def empty[A](dim : Int) : KDTree[A] = new KDTree[A](dim, Nil, 0)
}
