package utils.datastructures

import scala.collection.mutable.ArrayBuffer


object CircullarArray {
  implicit def toCyclic[A](a: ArrayBuffer[A]) = CircullarArray(a)

  def circullarIndex(element: Int, length: Int): Int =
    if (element >= 0) {
      element % length
    } else {
      (element % length) + length
    }
}

case class CircullarArray[A](a: ArrayBuffer[A]) {
  def getCircullar(id: Int): A = {
    if (id >= 0) {
      a(id % a.length)
    } else {
      a((id % a.length) + a.length)
    }
  }
}
