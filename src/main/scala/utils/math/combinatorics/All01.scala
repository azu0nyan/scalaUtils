package utils.math.combinatorics

object All01 {

  def all01NonEmpty(n: Int): Seq[String] = {
    for (i <- 0L until (1L << n)) yield {
      val s = i.toBinaryString
      "0".repeat(n - s.length) + s
    }
  }

  def withConsecutive0(n: Int): Seq[String] =
    if (n <= 1) Seq() else {
      for (
        i <- 0 until (n - 1);
        h <- if (i == 0) Seq("") else all01NonEmpty(i);
        t <- if (i == n - 2) Seq("") else all01NonEmpty(n - 2 - i)
      ) yield h + "00" + t
    }.toSet.toSeq

  def withExactlyTwo0(n: Int): Seq[String] = if (n <= 1) Seq() else
    for (
      i <- 0 until n;
      j <- (i + 1) until n
    ) yield "1".repeat(i) + "0" + "1".repeat(j - i - 1) + "0" + "1".repeat(n - 1 - j)

  def withAtLeastTwo0(n: Int): Seq[String] =
    all01NonEmpty(n).filter(x => x.count(_ == '0' ) >= 2 )


  def withAtLeast2ZerosAnd2Ones(n: Int): Seq[String] =
    all01NonEmpty(n).filter(x => x.count(_ == '0' ) >= 2 && x.count(_ == '1') >= 2)


  def with2ZerosXor2Ones(n:Int):Seq[String] = all01NonEmpty(n)
    .filter(x => x.count(_ == '0' ) == 2 ^ x.count(_ == '1') == 2)

}
