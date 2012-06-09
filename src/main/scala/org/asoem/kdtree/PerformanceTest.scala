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

class ConstructionBenchmark(title: String, n: Int, val dim: Int, threshold: Int = 0) extends TestCase {
  override def prefix = title + " KDtree Construction (dim=" + dim + ",threshold=" + t + ", #node=" + n + ")"

  var points : Seq[Product2[HyperPoint, Any]] = null

  def randomPoint() = HyperPoint(List.fill(dim) { Random.nextInt(10) + Random.nextDouble() })
  def randomTuple() = (randomPoint(), 0)
  def tuples() : Seq[Product2[HyperPoint, Any]] = List.fill(n) { randomTuple() }
  override def preRun() { points = tuples() }

  private val t = if (threshold == 0) KDTree.defaultThreshold(n) else threshold

  def run() {new KDTree(dim, points, t)}
}

object PerformanceTest {

  def median(list: List[Long]) = list.sorted.apply(list.size / 2)

  def main(args : Array[String]) {
    require(args.size == 3,
      "Usage: PerformanceTest <runs> <dim> <size>")

    println("Starting PerformanceTest")

    val runs = args(0).toInt
    val dim = args(1).toInt
    val size = args(2).toInt

    println("min, max, median [us] (runs=" + runs + ")")
    for (testCase <- List[TestCase](
      new ConstructionBenchmark("Threshold 1000", size, dim, 1000),
      new ConstructionBenchmark("Threshold 2000", size, dim, 2000),
      new ConstructionBenchmark("Threshold MAX", size, dim, Int.MaxValue),
      new ConstructionBenchmark("LinearSeq", size, dim),
      new ConstructionBenchmark("IndexedSeq", size, dim) {
        override def tuples = super.tuples().toIndexedSeq
      }
    ))
    {
      println(" - " + testCase.prefix + ": ")
      val result = testCase.runBenchmark(runs)
      println("\t" + ((result.min, result.max, median(result)).productIterator.map {_.toString} mkString (", ")))
    }
  }
}