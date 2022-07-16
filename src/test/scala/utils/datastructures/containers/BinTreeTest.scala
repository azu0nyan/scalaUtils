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

   assert(t.closestLessB[Int](0, (x, y) => x < y).contains(-1) )
    val t2 = t.remove(-1)
    assert(!t2.contains(-1))

  }
}
