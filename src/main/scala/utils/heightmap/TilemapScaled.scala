package utils.heightmap

import utils.datastructures.IntV2
import utils.math._

case class TilemapScaled(toScale: Tilemap, scale: Int) extends Tilemap {
  override def resolution: IntV2 = toScale.resolution * IntV2(scale, scale)

  override def valueAtUnsafe(pos: IntV2): Scalar = toScale.valueAt(pos / IntV2(scale, scale))
}

