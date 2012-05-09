package org.asoem.kdtree

import scala.actors.Futures._
import collection.mutable.{LinkedList, ListBuffer, Stack}
import scala.Int
import collection.Iterable

class KDTree[A](pointValueInput : Seq[KDTuple[A]], forkJoinThreshold : Int) extends HyperObject with Iterable[KDNode[A]] {

  require(pointValueInput != null, "Argument 'pointValueInput' must not be null")

  override val dim = pointValueInput.size match {
    case 0 => 0
    case 1 => pointValueInput.head.dim
    case _ => {
      require(pointValueInput.forall(e => e.dim == pointValueInput.head.dim),
        "All elements in 'pointValueInput' must have the same dimension")
      pointValueInput.head.dim
    }
  }

  override val size = pointValueInput.size

  private val _root = {

    val threshold =
      if (forkJoinThreshold == 0) math.max(pointValueInput.size, 1000) / Runtime.getRuntime.availableProcessors
      else forkJoinThreshold

    def append(sublist : Seq[KDTuple[A]], depth : Int = 0) : KDNode[A] = sublist.length match {
      case 0 => null
      case _ =>
        assert(dim != 0)

        val axis = if (dim == 2) depth & 1 else depth % dim
        val sublistSorted = sublist.sortBy(e => e.point(axis))
        val median = sublist.length / 2

        val appendLeft = () => append(sublistSorted.take(median), depth+1)
        val appendRight = () => append(sublistSorted.drop(median+1), depth+1)

        val doFork = (sublist.length > threshold)
        val resultLeft = if (doFork) future{appendLeft()} else appendLeft
        val resultRight = if (doFork) future{appendRight()} else appendRight

        new KDNode(
          sublistSorted(median),
          axis,
          resultLeft(),
          resultRight()
        )
    }

    if (dim == 0) null else append(pointValueInput)
  }
  def root : KDNode[A] = _root;

  def this() = this(Nil, 0)
  def this(pointValueInput : Seq[KDTuple[A]]) = this(pointValueInput, 0)
  def this(pointValueInput : Seq[HyperPoint], mapFunction : (HyperPoint) => A, forkJoinThreshold : Int = 0) =
    this(pointValueInput.map(e => new KDTuple(e, mapFunction(e))), forkJoinThreshold)
  def this(pointValueInput : Iterable[KDTuple[A]]) = this(pointValueInput toSeq)

  def findNeighbours(searchPoint : HyperPoint,
                     nNeighbours: Int = Integer.MAX_VALUE,
                     searchRange : Double = Double.MaxValue) : List[NNResult[A]] = {

    require(searchPoint != null, "Argument 'searchPoint' must not be null")
    require(searchPoint.dim == dim, "Dimension of 'searchPoint' (%d) does not match dimension of this tree (%d).".format(searchPoint.dim, dim))
    require(searchRange >= 0, "Argument searchRange must not be negative: " + searchRange)

    if (nNeighbours == 0) // TODO: should maybe produce at least a log message
      return Nil

    var resultList = LinkedList[NNResult[A]]()

    def insertWhere[T](list: LinkedList[T], elem: T, p: (T) => Boolean) : LinkedList[T] = {
      require( list != null && elem != null && p != null)

      if ( list.isEmpty )
        new LinkedList[T](elem, null)

      else if (p.apply(list.elem))
        new LinkedList(elem, list)

      else list.append(insertWhere[T]( list.next, elem, p))
    }

    def search(node : KDNode[A]) {
      if (node != null) {
        val dist = searchPoint.distance(node.point)

        if (dist <= searchRange) {
          val element = new NNResult(node.point, node.value, dist)
          //resultList = insertWhere[NNResult[A]](resultList, element, (e) => { assume(e != null); (element compare e) <= 0})
          resultList = resultList.append(new LinkedList(element, null))
        }

        if (!node.isLeaf) {

          val distance = searchPoint(node.splitDim) - node.point(node.splitDim)
          val closeChild = if (distance <= 0) node.left else node.right
          val farChild = if (distance <= 0) node.right else node.left

          // search in the HyperRect containing searchPoint
          search(closeChild)

          // search in the HyperRect not containing searchPoint
          // if it intersects with searchRange
          if (math.abs(distance) <= searchRange)
            search(farChild)
        }
      }
    }

    search(_root)

    (resultList take nNeighbours).toList
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
      val sorted = pointValueInput.sortWith((e1, e2) => e1.point(i) < e2.point(i))
      min = min.edit(i, math.min(min(i), sorted.head.point(i)))
      max = max.edit(i, math.max(max(i), sorted.last.point(i)))
    }
    HyperRect(min, max)
  }
}

