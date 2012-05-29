package org.asoem.kdtree

import scala.util.Random
import scala.testing.Benchmark

abstract class TestCase extends Benchmark {
  override def setUp() {}

  def preRun() {}
  def postRun() {}

  override def runBenchmark(noTimes : Int) : List[Long] = {
    setUp()
    List.fill(noTimes)(wrappedRun())
  }

  final def wrappedRun() : Long = {
    preRun()

    val startTime = System.nanoTime
    run()
    val ret = (System.nanoTime - startTime) / 1000    // ns to ms

    postRun()
    ret
  }
}

object PerformanceTest {
  val N = 10000
  val dim = 2

  val points = List.fill(N)(HyperPoint(List.fill(dim)(Random.nextInt(10) + Random.nextDouble())))
  val tree = new KDTree[Int](points.map(e => (e, 0)))

  var searchPoints : List[HyperPoint] = null

  val kdtreeBenchmark100 = new TestCase {
    override def prefix = "KDtree Construction (threshold=100, #node=" + N + ")"

    var myPoints : List[HyperPoint] = null
    override def preRun() {
      myPoints = List.fill(N)(HyperPoint(List.fill(dim)(Random.nextInt(10) + Random.nextDouble())))
    }
    def run() {new KDTree(points.map(e => (e, 0)), 100)}
  }

  val kdtreeBenchmark500 = new TestCase {
    override def prefix = "KDtree Construction (threshold=500, #node=" + N + ")"

    var myPoints : List[HyperPoint] = null
    override def preRun() {
      myPoints = List.fill(N)(HyperPoint(List.fill(dim)(Random.nextInt(10) + Random.nextDouble())))
    }
    def run() {new KDTree(points.map(e => (e, 0)), 500)}
  }

  val kdtreeBenchmark1000 = new TestCase {
    override def prefix = "KDtree Construction (threshold=1000, #node=" + N + ")"

    var myPoints : List[HyperPoint] = null
    override def preRun() {
      myPoints = List.fill(N)(HyperPoint(List.fill(dim)(Random.nextInt(10) + Random.nextDouble())))
    }
    def run() {new KDTree(points.map(e => (e, 0)), 1000)}
  }

  val kdtreeBenchmark5000 = new TestCase {
    override def prefix = "KDtree Construction (threshold=5000, #node=" + N + ")"

    var myPoints : List[HyperPoint] = null
    override def preRun() {
      myPoints = List.fill(N)(HyperPoint(List.fill(dim)(Random.nextInt(10) + Random.nextDouble())))
    }
    def run() {new KDTree(points.map(e => (e, 0)), 5000)}
  }

  val kdtreeBenchmark10000 = new TestCase {
    override def prefix = "KDtree Construction (threshold=10000, #node=" + N + ")"

    var myPoints : List[HyperPoint] = null
    override def preRun() {
      myPoints = List.fill(N)(HyperPoint(List.fill(dim)(Random.nextInt(10) + Random.nextDouble())))
    }
    def run() {new KDTree(points.map(e => (e, 0)), 10000)}
  }

  val kdtreeBenchmarkDefault = new TestCase {
    override def prefix = "KDtree Construction (threshold=default, #node=" + N + ")"

    var myPoints : List[HyperPoint] = null
    override def preRun() {
      myPoints = List.fill(N)(HyperPoint(List.fill(dim)(Random.nextInt(10) + Random.nextDouble())))
    }
    def run() {new KDTree(points.map(e => (e, 0)))}
  }

  val kdtreeBenchmarkSequential = new TestCase {
    override def prefix = "KDtree Construction (threshold=Max, #node=" + N + ")"

    var myPoints : List[HyperPoint] = null
    override def preRun() {
      myPoints = List.fill(N)(HyperPoint(List.fill(dim)(Random.nextInt(10) + Random.nextDouble())))
    }
    def run() { new KDTree(points.map(e => (e, 0)), Int.MaxValue)}
  }

  val findNeighboursImmutableBenchmark = new TestCase {
    override def prefix = "findNeighbours (radius=" + radius + ", treeSize=" + N + ")"
    var nnList : List[NNResult[Int]] = Nil
    var radius : Double = 1
    var iter : Iterator[HyperPoint] = null

    override def setUp() { iter = searchPoints.iterator }
    override def preRun() { assert(iter != null && iter.hasNext) }
    def run() {nnList = tree.filterRange(iter.next(), radius)}
    override def postRun() {}
  }

  val reportNodes2Benchmark = new TestCase {
    override def prefix = "toList"
    override def run() {tree.toList}
  }

  def median(list: List[Long]) = list.sorted.apply(list.size / 2)

  def main(args : Array[String]) {
    require(args.size == 2,
      "Usage: PerformanceTest <runs> <serachRange>")

    println("Starting PerformanceTest")

    val runs = args(0).toInt
    searchPoints = List.fill(runs)(HyperPoint(List.fill(dim)(Random.nextDouble())))

    //    nnSearchBenchmark.radius = args(1).toDouble
    //    findNeighboursMutableBenchmark.radius = args(1).toDouble
    findNeighboursImmutableBenchmark.radius = args(1).toDouble


    println("min, max, median [us] (runs=" + runs + ")")
    for (testCase <- List[TestCase](
      //      kdtreeBenchmark100,
      //      kdtreeBenchmark500,
      //      kdtreeBenchmark1000,
      //      kdtreeBenchmark5000,
      //      kdtreeBenchmark10000,
      //      kdtreeBenchmarkDefault,
      //      kdtreeBenchmarkSequential
      //      reportNodes2Benchmark,
            findNeighboursImmutableBenchmark
    ))
    {
      println(" - " + testCase.prefix + ": ")
      val result = testCase.runBenchmark(runs)
      println("\t" + ((result.min, result.max, median(result)).productIterator.map {_.toString} mkString (", ")))
    }
  }
}