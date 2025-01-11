package utils.datastructures

import scala.collection.mutable.ArrayBuffer


object CircullarOps {
  implicit def toCyclic[A](a: Seq[A]):CircullarSeq[A] =new CircullarSeq(a)

  implicit def toCyclic[A](a: Array[A]):CircullarArray[A] =new CircullarArray(a)

  def toCyclicPairs[A](a: Seq[A]): Iterator[(A, A)] = {
    (a :+ a.head).sliding(2).map{case Seq(a, b) => (a, b)}
  }

  @inline def circullarIndex(element: Int, length: Int): Int = utils.math.circullarIndex(element, length)


  implicit class CircullarSeq[A](a: Seq[A]) {
    def getCircullar(id: Int): A = a(circullarIndex(id, a.length))
  }

  implicit class CircullarArray[A](a: Array[A]) {
    def getCircullar(id: Int): A = a(circullarIndex(id, a.length))
    def setCircullar(el:A, id: Int): Unit = a(circullarIndex(id, a.length)) = el
  }
  
  
  extension [A](a: Seq[A]) {
    def asCyclicPairs: Iterator[(A, A)] = toCyclicPairs(a)
  }

}



