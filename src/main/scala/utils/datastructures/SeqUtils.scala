package utils.datastructures

object SeqUtils {


  implicit class CircullarSeq[A](a:Seq[A]) {

    def getCircullar(id: Int): A = {
      if (id >= 0) {
        a(id % a.length)
      } else {
        a((id % a.length) + a.length)
      }
    }

    def toCyclicPairs: Seq[(A, A)] = a.indices.map(i => (a.getCircullar(i), a.getCircullar(i + 1)))
  }


  //implicit def toCyclic[A](a: Seq[A]) = IndexedSeq(a)



  def circullarIndex(element: Int, length: Int): Int =
    if (element >= 0) {
      element % length
    } else {
      (element % length) + length
    }

  def clamp[T](seq: Seq[T], min: T, max: T)(implicit ev$1: T => Ordered[T]): Seq[T] = seq.filter(x => x >= min && x <= max)

  def clampAndEnsureMinMaxExistence[T](seq: Seq[T], min: T, max: T)(implicit ev$1: T => Ordered[T]): Seq[T] =
    (clamp(seq, min, max) match { //add min if missing
      case result@head +: _ if head != min => min +: result
      case result@head +: _ if head == min => result
      case _ => Seq(min)
    }) match { //add max if missing
      case result@_ :+ tail if tail != max => result :+ max
      case result@_ :+ tail if tail == max => result
      case _ => Seq(max)
    }

}
