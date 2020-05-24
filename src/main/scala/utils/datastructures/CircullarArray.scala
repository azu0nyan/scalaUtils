package utils.datastructures

import scala.collection.mutable.ArrayBuffer


object CircullarArray {
  implicit def toCyclic[A](a: Seq[A]):CircullarSeq[A] = CircullarSeq(a)

  @inline def circullarIndex(element: Int, length: Int): Int =
    if (element >= 0) {
      element % length
    } else {
      (element % length) + length
    }
}

case class CircullarMap[A](a: ArrayBuffer[A], from0Until:Int)extends (Int => A){
  override def apply(v1: Int): A = super.apply(CircullarArray.circullarIndex(v1, from0Until))
}

case class CircullarSeq[A](a: Seq[A]) {
  def getCircullar(id: Int): A = a(CircullarArray.circullarIndex(id, a.length))
}

case class CircullarArray[A](a: Array[A]) {
  def getCircullar(id: Int): A = a(CircullarArray.circullarIndex(id, a.length))
  def setCircullar(el:A, id: Int): Unit = a(CircullarArray.circullarIndex(id, a.length)) = el
}
