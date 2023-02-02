package utils.math.misc

import utils.math._

object IntervalOps {


  /** Cuts intervals from given intervals in O(n*log(n)) assumes that for every interval [l, r] l < r */
  def cutFrom(from: (Scalar, Scalar), toCut: Seq[(Scalar, Scalar)]): Seq[(Scalar, Scalar)] = {
    import utils.math.WithAlmostEquals
    toCut
      .map { case (l, r) => (clamp(l, from._1, from._2), clamp(r, from._1, from._2)) }
      .filter { case (l, r) => l !~= r } //filter out empty
      .sorted
      .foldLeft(Seq(from)) {
        case (first :+ ((ll, lr)), (l, r)) => //ll - lastl lr - lastr
          if (ll ~< l && r ~< lr) first :+ (ll, l) :+ (r, lr)
          else if ((ll ~= l) && r ~< lr) first :+ (r, lr)
          else if (ll ~< l && (r ~= lr)) first :+ (ll, l)
          else if ((ll ~= l) && (r ~= lr)) first
          else if (l ~< ll && r ~< lr) first :+ (r, lr)
          else if (l ~< ll && (r ~<= lr)) first
          else if (r ~<= ll) first :+ (ll, lr)
          else throw new RuntimeException(s"Cant cut from $from -> $toCut")
        //other cases shouldn't be possible since toCut is sorted
      }
  }
}
