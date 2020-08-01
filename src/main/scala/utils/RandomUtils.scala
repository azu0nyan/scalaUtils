package utils

import scala.util.Random

object RandomUtils extends RandomUtils

trait RandomUtils {

  def randomFromSeq[T](seq: Seq[T])(implicit seed: Int): T = seq(new Random(seed).nextInt(seq.size))
}
