package utils.datastructures.containers

import org.scalatest.funsuite.AnyFunSuite
import utils.datastructures.containers.BinaryTree.EmptyTree

class BinTreeTest extends AnyFunSuite {
  test("some tests") {
    val t = EmptyTree.add(1).add(2).add(5).add(3).add(-1)
    assert(t.contains(1))
    assert(t.contains(2))
    assert(t.contains(5))
    assert(t.contains(3))
    assert(t.contains(-1))
    assert(t.size == 5)

    assert(t.maximumSatisfiesCondition(_ < -1).isEmpty)
    assert(t.maximumSatisfiesCondition(_ < 0).contains(-1))
    assert(t.maximumSatisfiesCondition(_ < 2).contains(1))
    assert(t.maximumSatisfiesCondition(_ < 3).contains(2))
    assert(t.maximumSatisfiesCondition(_ < 5).contains(3))
    assert(t.maximumSatisfiesCondition(_ < 6).contains(5))

    assert(t.maximumSatisfiesCondition(_ <= -2).isEmpty)
    assert(t.maximumSatisfiesCondition(_ <= -1).contains(-1))
    assert(t.maximumSatisfiesCondition(_ <= 0).contains(-1))
    assert(t.maximumSatisfiesCondition(_ <= 2).contains(2))
    assert(t.maximumSatisfiesCondition(_ <= 3).contains(3))
    assert(t.maximumSatisfiesCondition(_ <= 4).contains(3))
    assert(t.maximumSatisfiesCondition(_ <= 5).contains(5))
    assert(t.maximumSatisfiesCondition(_ <= 6).contains(5))


    val t2 = t.remove(-1)
    assert(!t2.contains(-1))

  }
}
