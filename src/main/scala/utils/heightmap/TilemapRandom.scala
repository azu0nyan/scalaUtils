package utils.heightmap

import utils.datastructures.IntV2

import scala.util.Random
import utils.math._

case class TilemapRandom(wh:IntV2, seed:Int = (Math.random() * Integer.MAX_VALUE).toInt) extends Tilemap {
  override def resolution: IntV2 = wh

  override def valueAtUnsafe(pos: IntV2): Scalar = new Random(new Random(new Random(seed + toFlatHeightIndex(pos)).nextInt()).nextInt()).nextFloat()



}
