package utils

import scala.util.Random

object RandomUtils extends RandomUtils

trait RandomUtils {

  def randomInt(fromInc:Int, toUninc:Int)(implicit seed: Int):Int = new Random(seed).nextInt(toUninc - fromInc) + fromInc

  def randomFromSeq[T](seq: Seq[T])(implicit seed: Int): T = seq(new Random(seed).nextInt(seq.size))
}
