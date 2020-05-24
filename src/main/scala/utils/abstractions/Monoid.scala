package utils.abstractions

object Monoid{

}

trait Monoid[T] {
  def empty:T

  def combine(a:T, b:T):T

  def combineAll(seq: Iterable[T]): T = seq.reduce(combine)
}
