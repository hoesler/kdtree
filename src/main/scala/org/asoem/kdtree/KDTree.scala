package org.asoem.kdtree

import scala.actors.Futures._
import scala.collection.Iterable
import scala.collection.mutable.{LinkedList, Stack}
import scala._

class KDTree[+A](pointValueInput : Seq[Product2[HyperPoint, A]], forkJoinThreshold : Int) extends HyperObject with Iterable[KDNode[A]] {

  require(pointValueInput != null, "Argument 'pointValueInput' must not be null")

  override val dim = pointValueInput.size match {
    case 0 => 0
    case 1 => pointValueInput.head._1.dim
    case _ => {
      val dimFirstElement: Int = pointValueInput.head._1.dim
      require(pointValueInput.forall(e => { e._1.dim == dimFirstElement }),
        "All elements in 'pointValueInput' must have the same dimension")
      dimFirstElement
    }
  }

  override val size = pointValueInput.size

  private val _root = {

    val threshold =
      if (forkJoinThreshold == 0) math.max(pointValueInput.size, 1000) / Runtime.getRuntime.availableProcessors
      else forkJoinThreshold

    val splitAxisFunction = if (dim == 2) (depth:Int) => {depth & 1} else (depth:Int) => {depth % dim}

    def append(sublist : Seq[Product2[HyperPoint, A]], depth : Int = 0) : KDNode[A] = sublist.length match {
      case 0 => null
      case 1 => KDNode(sublist.head, splitAxisFunction(depth), null, null)
      case _ =>
        assert(dim != 0)

        val axis = splitAxisFunction(depth)
        val indexOfSplit = sublist.length / 2
        val (left, rightWithMedian) = sublist.sortBy(e => e._1(axis)).splitAt(indexOfSplit)

        val newDepth: Int = depth + 1
        val appendLeft = () => append(left, newDepth)
        val appendRight = () => append(rightWithMedian.tail, newDepth)

        val doFork = (sublist.length > threshold)
        val resultLeft = if (doFork) future{appendLeft()} else appendLeft
        val resultRight = if (doFork) future{appendRight()} else appendRight

        KDNode(
          rightWithMedian.head,
          axis,
          resultLeft(),
          resultRight()
        )
    }

    if (dim == 0) null else append(pointValueInput)
  }
  def root : KDNode[A] = _root

  def this() = this(Nil, 0)
  def this(pointValueInput : Seq[Product2[HyperPoint, A]]) = this(pointValueInput, 0)
  def this(pointValueInput : Iterable[Product2[HyperPoint, A]]) = this(pointValueInput toSeq)

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

    search(_root)
  }

  def findNeighbours(searchPoint : HyperPoint, k: Int = 1) : List[NNResult[A]] = {

    require(searchPoint != null, "Argument 'searchPoint' must not be null")
    require(searchPoint.dim == dim, "Dimension of 'searchPoint' (%d) does not match dimension of this tree (%d).".format(searchPoint.dim, dim))
    require(k <= size, "k=%d must be <= size=%d".format(k, size))

    if (k == 0) // TODO: should maybe produce at least a log message
      return Nil

    var resultList = LinkedList[NNResult[A]]()
    var farNode = LinkedList[NNResult[A]]()

    def search(node : KDNode[A]) {
      if (node != null) {
        val distanceToSearchPoint = searchPoint.distance(node.point)

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
    }

    search(_root)

    resultList.toList
  }

  override def iterator: Iterator[KDNode[A]] =
    new Iterator[KDNode[A]] {
      val nodeStack = Stack(_root)

      override def hasNext = {
        ! nodeStack.isEmpty && ! nodeStack.head.isLeaf
      }

      override def next() = {
        if (hasNext) {
          val node = nodeStack.pop()
          if (node.right != null)
            nodeStack push node.right
          if (node.left != null)
            nodeStack push node.left
          nodeStack.head
        }
        else
          throw new NoSuchElementException
      }
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

