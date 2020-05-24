package utils.datastructures

import scala.collection.mutable.ArrayBuffer


object CircullarSeq {
  implicit def toCyclic[A](a: Seq[A]) = IndexedSeq(a)

  implicit def toCyclicPairs[A](a: Seq[A]): Seq[(A, A)] = a.indices.map(i => (a.getCircullar(i), a.getCircullar(i + 1)))

  implicit class CircullarIndexedSeq[A](a: Seq[A]) {
    def getCircullar(id: Int): A = {
      if (id >= 0) {
        a(id % a.length)
      } else {
        a((id % a.length) + a.length)
      }
    }
  }

}

