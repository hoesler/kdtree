package org.asoem.kdtree

trait HyperObject {
   def dim : Int
   def boundingBox : HyperRect
}