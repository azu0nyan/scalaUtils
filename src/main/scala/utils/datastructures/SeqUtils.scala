package utils.datastructures

object SeqUtils {

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
