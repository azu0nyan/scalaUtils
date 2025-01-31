package utils.datastructures.containers

import org.scalatest.AppendedClues.*
import org.scalatest.funsuite.AnyFunSuite
import utils.math.combinatorics.Permutations

class BinHeapTest extends AnyFunSuite {

  test("bug"){
    val b:BinHeap[Int] = new BinHeap[Int]
    b.add(2)
    b.add(1)

    assert(b.poll() == 1)
  }

  test("add take 1"){
    val b:BinHeap[Int] = new BinHeap[Int]
    b.add(1)
    assert(b.poll() == 1)
  }

  test("add take 2"){
    val b:BinHeap[Int] = new BinHeap[Int]
    b.add(1)
    b.add(2)
    assert(b.poll() == 1)
    assert(b.poll() == 2)
    b.add(2)
    b.add(1)

    assert(b.poll() == 1)
    assert(b.poll() == 2)
  }

  test("add take n"){
    val b:BinHeap[Int] = new BinHeap[Int]
    for(i <- 1 to 6) {
      Permutations.allPermutations(i).foreach { p =>
        p.foreach(i => b.add(i))
        val res = b.takeAllIterator().toSeq
        val shouldBe = ( 0 until i).toSeq
        assert(res == shouldBe) withClue s"add order $p result $res should be $shouldBe"
      }
    }
  }

  test("bug2"){
    val b:BinHeap[Int] = new BinHeap[Int]
    b.add(0)
    b.add(1)
    b.add(3)
    b.add(2)
    b.add(4)
    b.add(5)

    assert(b.poll() == 0)
    assert(b.poll() == 1)
    assert(b.poll() == 2)
    assert(b.poll() == 3)
    assert(b.poll() == 4)
    assert(b.poll() == 5)
  }

  test("change priority"){
    val b:BinHeap[Int] = new BinHeap[Int]
    b.add(0)
    b.add(1)
    b.add(3)
    b.update(0, 5)
    assert(b.poll() == 1)
    assert(b.poll() == 3)
    assert(b.poll() == 5)

  }


//  test("add take n int bin heap"){
//    val b:IntBinHeap = new IntBinHeap((x, y) => x < y)
//    for(i <- 1 to 6) {
//      Permutations.allPermutations(i).foreach { p =>
//        p.foreach(i => b.add(i))
//        val res = b.takeAllIterator().toSeq
//        val shouldBe = ( 0 until i).toSeq
//        assert(res == shouldBe) withClue s"add order $p result $res should be $shouldBe"
//      }
//    }
//  }

}
