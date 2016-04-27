kdtree
======

[![Build Status](https://travis-ci.org/hoesler/kdtree.svg?branch=master)](https://travis-ci.org/hoesler/kdtree)

A [Scala](http://www.scala-lang.org/) library containing a [k-d tree](http://en.wikipedia.org/wiki/K-d_tree) implementation.

Description
-----------

Trees are immutable. Construction is done asynchronously using a fork-join approach.

Usage
-----

```Scala
val nodes: Seq[(HyperPoint, String)] = List(
    (HyperPoint(2, 3), "A"),
    (HyperPoint(5, 4), "B")
)

val tree = KDTree(nodes)

tree.filterRange(HyperSphere(HyperPoint(7, 2), 1.5))
tree.findNeighbours(HyperPoint(9.1, 6.1), k = 1)
```
