package utils

package object sugar {
  implicit class RichInt(val value: Int) extends AnyVal {
    def downto (n: Int): Range = value to n by -1
    def downtil (n: Int): Range = value until n by -1
  }

  implicit class Vectorization[T](vec:(T,T,T)){
    def x:T = vec._1
    def y:T = vec._2
    def z:T = vec._3
  }

  implicit class ListWithInsert[T](val list: List[T]) extends AnyVal {
    def insert(i: Int, values: T*): List[T] = {
      val (front, back) = list.splitAt(i)
      front ++ values ++ back
    }
  }
}
