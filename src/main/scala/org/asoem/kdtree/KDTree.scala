package org.asoem.kdtree

import annotation.tailrec
import scala.actors.Futures._
import collection.Iterable

class KDTree[A](pointValueInput : Seq[KDTuple[A]], forkJoinThreshold : Int) extends HyperObject {

  require(pointValueInput != null, "PointList is NULL")

  override def dim = if (pointValueInput.isEmpty) 0 else pointValueInput(0).dim

  require(pointValueInput.forall(e => e.dim == dim), "All Points must have the same dimension")

  private val threshold : Int =
    if (forkJoinThreshold == 0) math.max(pointValueInput.length, 1000) / Runtime.getRuntime().availableProcessors
    else forkJoinThreshold

  private val root = buildTree(pointValueInput)

  lazy val boundingBox = calculateBoundingBox

  lazy val size = toList.size

  def this() = this(Nil, 0)
  def this(pointValueInput : Seq[KDTuple[A]]) = this(pointValueInput, 0)
  def this(pointValueInput : Seq[HyperPoint], mapFunction : (HyperPoint) => A, forkJoinThreshold : Int = 0) =
    this(pointValueInput.map(e => KDTuple(e, mapFunction(e))), forkJoinThreshold)
  def this(pointValueInput : Iterable[KDTuple[A]]) = this(pointValueInput toSeq)

  private def buildTree(list : Seq[KDTuple[A]]) : KDNode[A] = {
    def append(sublist : Seq[KDTuple[A]], depth : Int, useFuture : Boolean = false) : KDNode[A] = sublist.length match {
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

    append(list, 0, true)
  }

  override def toString : String = {
    if (root == null)
      return "null"
    else {
      return traverse(root, 0, "")
    }
  }

  private def traverse(node : KDNode[A], depth : Int, prefix : String) : String = {

    val str = new StringBuilder

    str.append(prefix)
    if (node != null)
      str.append(node)
    else
      str.append("0")
    str.append("\n")

    val treePrefix =
      if (prefix.isEmpty) ""
      else prefix.dropRight(3).+(
        if (prefix.charAt(prefix.length - 3) == '|') "|  "
        else "   "
      )

    if (node != null && ! node.isLeaf) {
      str.append(traverse(node.right, depth+1, treePrefix + "|--"))
      str.append(traverse(node.left, depth+1, treePrefix + "`--"))
    }

    return str.toString
  }

  def findNeighbours(searchPoint : HyperPoint,
                     max: Int = Integer.MAX_VALUE,
                     range : Double = Double.MaxValue) : List[NNResult[A]] = {
    //@tailrec
    def combinedSearch(node : KDNode[A],
                       searchPoint : HyperPoint,
                       range : Double,
                       list : List[NNResult[A]] = Nil) : List[NNResult[A]] = node match {
      case null => list

      case _ =>
      {
        val dist = searchPoint.distance(node.point)

        val updatedList =
          if (dist <= range) {
            val element = NNResult(node.point, node.value, dist)
            element :: list
          }
          else list

        if (node.isLeaf)
          return updatedList

        val dx = searchPoint(node.splitDim) - node.point(node.splitDim)
        val closeChild = if (dx <= 0) node.left else node.right
        val farChild = if (dx <= 0) node.right else node.left

        // search in the HyperRect not containing searchPoint
        // if it intersects with the search range
        val listFar =
          if (math.abs(dx) <= range)
            combinedSearch( farChild, searchPoint, range, updatedList )
          else
            updatedList

        // search in the HyperRect containing searchPoint
        combinedSearch( closeChild, searchPoint, range, listFar )
      }
    }

    require(searchPoint != null)
    require(searchPoint.dim == dim)

    if (max == 0) // TODO: should maybe produce at least a log message
      return Nil

    val sortedResultList = combinedSearch(root, searchPoint, range) sorted

    if (sortedResultList.size > max)
      sortedResultList take max
    else
      sortedResultList
  }

  def toList : List[KDNode[A]] = {
    collectNodes(root)
  }

  //@tailrec
  private def collectNodes(node : KDNode[A], list : List[KDNode[A]] = Nil) : List[KDNode[A]] = node match {
    case null           => list
    case _              => {
      val newList = node :: list
      collectNodes(node.right, collectNodes(node.left, newList))
    }
  }

  private def calculateBoundingBox() = {
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

