package utils.datastructures

import scala.collection.mutable.ArrayBuffer


object CircullarOps {
  implicit def toCyclic[A](a: Seq[A]):CircullarSeq[A] =new CircullarSeq(a)

  implicit def toCyclic[A](a: Array[A]):CircullarArray[A] =new CircullarArray(a)

  implicit def toCyclicPairs[A](a: Seq[A]): Seq[(A, A)] = a.indices.map(i => (a(circullarIndex(i, a.length)), a(circullarIndex(i + 1, a.length))))

  @inline def circullarIndex(element: Int, length: Int): Int = utils.math.circullarIndex(element, length)


  implicit class CircullarSeq[A](a: Seq[A]) {
    def getCircullar(id: Int): A = a(circullarIndex(id, a.length))
  }

  implicit class CircullarArray[A](a: Array[A]) {
    def getCircullar(id: Int): A = a(circullarIndex(id, a.length))
    def setCircullar(el:A, id: Int): Unit = a(circullarIndex(id, a.length)) = el
  }

}



