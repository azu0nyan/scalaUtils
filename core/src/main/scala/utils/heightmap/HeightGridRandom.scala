package utils.heightmap

import utils.datastructures.IntV2
import utils.math.*

import scala.util.Random

case class HeightGridRandom(wh:IntV2, seed:Int = (Math.random() * Integer.MAX_VALUE).toInt) extends HeightGrid {
  override def resolution: IntV2 = wh

  override def valueAtUnsafe(pos: IntV2): Scalar = new Random(new Random(new Random(seed + toFlatHeightIndex(pos)).nextInt()).nextInt()).nextFloat()



}
