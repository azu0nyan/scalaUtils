package utils.datastructures

import org.scalatest.funsuite.AnyFunSuite
import utils.datastructures.containers.DisjointSet

class DisjointSetTest extends AnyFunSuite {
  test("Basic Ops") {
    val s = new DisjointSet[Int]
    s.makeSet(1)
    assert(s.findSet(1) == 1)
    s.makeSet(2)
    assert(s.findSet(2) == 2)
    s.makeSet(3)
    assert(s.findSet(3) == 3)
    s.union(2, 3)
    assert(s.findSet(2) == s.findSet(3))
    assert(s.findSet(1) != s.findSet(2))
    assert(s.findSet(1) != s.findSet(3))
    s.union(1, 3)
    assert(s.findSet(1) == s.findSet(3))
    assert(s.findSet(1) == s.findSet(2))
    assert(s.findSet(2) == s.findSet(3))


  }

  test("More ops") {
    val s = new DisjointSet[Int]
    for (i <- 0 to 100) {
      s.makeSet(i)
    }
    for (i <- 1 to 50) {
      if (i % 2 == 0) s.union(i - 1, i)
      else s.union(i, i - 1)
    }
    for (i <- 51 until 100) {
      if (i % 2 == 0) s.union(i + 1, i)
      else s.union(i, i + 1)
    }
    for(i <- 0 to 50; j <- 0 to 50){
      assert(s.findSet(i) == s.findSet(j))
    }
    for(i <- 51 to 100; j <- 51 to 100){
      assert(s.findSet(i) == s.findSet(j))
    }
    for(i <- 0 to 50; j <- 51 to 100){
      assert(s.findSet(i) != s.findSet(j))
    }
  }
}
