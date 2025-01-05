package utils.math.planar.algo.straightSkeleton.helpers

class Pair[A, B](
                  private var element1: A,
                  private var element2: B,
                ) {

  def first = this.element1

  def second = this.element2

  override def toString = "(" + this.element1 + "," + this.element2 + ")"

  def set1(element1: A): Unit = {
    this.element1 = element1
  }

  def set2(element2: B): Unit = {
    this.element2 = element2
  }

  override def equals(obj: Any) = try {
    val other = obj.asInstanceOf[Pair[?, ?]]
    other.element1 == this.element1 && other.element2 == this.element2
  } catch {
    case var3: ClassCastException =>
      false
  }

  override def hashCode = ((this.element1.hashCode.toLong + this.element2.hashCode.toLong) % 2147483647L).toInt
}
