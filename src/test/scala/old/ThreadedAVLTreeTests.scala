package old

import org.scalatest.AppendedClues._
import org.scalatest.FunSuite
import org.scalatest.funsuite.AnyFunSuite
import utils.datastructures.containers.ThreadedAVLTree.ThreadedAVLTree
import utils.math._
import utils.math.planar.{Rectangle, V2}

import scala.collection.mutable
import scala.util.Random


class ThreadedAVLTreeTests extends AnyFunSuite {


  def newDefaultInstance(): ThreadedAVLTree[Int] = new ThreadedAVLTree[Int]()

  test("add 1 tests reverse order") {
    val t: ThreadedAVLTree[Int] = newDefaultInstance()
    assert(t.notContains(1))
    t.add(6)
    t.add(5)
    t.add(4)
    t.add(3)
    t.add(2)
    t.add(1)
    assert(t.contains(1))
    assert(t.contains(2))
    assert(t.contains(3))
    assert(t.contains(4))
    assert(t.contains(5))
    assert(t.contains(6))
  }

  test("add 1 tests") {
    val t: ThreadedAVLTree[Int] = newDefaultInstance()
    assert(t.notContains(1))
    t.add(1)
    t.add(2)
    t.add(3)
    t.add(4)
    t.add(5)
    t.add(6)
    assert(t.contains(1))
    assert(t.contains(2))
    assert(t.contains(3))
    assert(t.contains(4))
    assert(t.contains(5))
    assert(t.contains(6))
  }

  test("add many test") {
    val t = newDefaultInstance()
    val seq = 0 until 1000000
    for (i <- seq) {
      t.add(i)
    }
    assert(t.size == seq.size)

    for (i <- seq) {
      assert(t.contains(i)) withClue s"$i not found"
    }
  }

  test("height tests") {
    for(k <- 0 until(40)) {
      val r = new Random(k)
      var i = 100 * k
      val t = newDefaultInstance()
      for (j <- 0 until i) {
        t.add(r.nextInt(100000))
      }

      t.traverseNodes(node => {
        val lHeight = t.height(node.left)
        val rHeight = t.height(node.right)
        assert(node.balanceFactor == (lHeight - rHeight)) withClue s"${node.balanceFactor} l:$lHeight r:$rHeight"
      })

    }
  }

  test("delete size "){
    val t = newDefaultInstance()
    for(i <- 0 until 50){
      t.add(i)
    }
    for(i <- 0 until 50){
      println(s"deleting $i")
      t.remove(i)
    }
    assert(t.size == 0)
    assert(t.values.toSeq.isEmpty)
  }

  test("delete  "){
    val t = newDefaultInstance()
    for(i <- 0 until 5000){
      t.add(i)
    }
    t.remove(4000)
    assert(t.notContains(4000))
    assert(t.notContains(-4000))
    assert(t.notContains(40000))
    assert(t.contains(3999))
    assert(t.contains(4999))
    for(i <- 1000 until 5000){
      t.remove(i)
    }
    assert(t.size == 1000)
    for(i <- 0 until 1000){
      t.remove(i)
    }
    assert(t.values.toSeq.isEmpty)
  }

  test("delete random ") {
    val t = newDefaultInstance()
    val control = new mutable.HashSet[Int]()
    val r = new Random(2)
    for(i <- 0 to 30000){
      if(r.nextDouble() > 0.25) {
        val x = r.nextInt(1000)
        println(s"add $x")
        t.add(x)
        control += x
      } else {
        val x = r.nextInt(1000)
        println(s"remove $x")
        t.remove(x)
        control -= x
      }
      t.traverseNodes(node => {
        val lHeight = t.height(node.left)
        val rHeight = t.height(node.right)
        assert(node.balanceFactor == (lHeight - rHeight)) withClue s"${node.balanceFactor} l:$lHeight r:$rHeight"
      })
      assert(t.values.toSet == control.toSet)
    }
  }
}
