package utils.math

import org.scalatest.funsuite.AnyFunSuite
import utils.math.combinatorics.{BitSet, Combinations, Partitions}

class Combinatorics extends AnyFunSuite {
  test("partitions") {
    val cur = Partitions.first(16)
    println(cur.mkString(" + "))
    while (Partitions.next(cur)) println(cur.filter(_ != 0).mkString(" + "))

  }

  test("Combinations") {
    val cur = Combinations.first(10, 3)
    println(cur.mkString(" "))
    while (Combinations.next(cur, 10)) {
      println(cur.mkString(" "))
    }

  }

  test("BitSet") {
    val cur = BitSet.first(10)
    println(cur.map(if (_) 1 else 0) mkString (""))
    while (BitSet.hasNext(cur)) {
      BitSet.next(cur)
      println(cur.map(if (_) 1 else 0) mkString (""))
    }

  }

}
