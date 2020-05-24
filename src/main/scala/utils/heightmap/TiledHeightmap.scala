package utils.heightmap

import utils.datastructures.IntV2
import utils.math.planar.V2
import utils.math._

trait TiledHeightmap extends Tilemap with Heightmap {
  override def heightAt(pos: V2): Scalar = {
    val cell = clampResolutionIndices(pos.toIntV2)
    val unit = V2(pos.x % 1f, pos.y % 1f)
    val h00 = valueAt(cell + IntV2(0, 0))
    val h01 = valueAt(cell + IntV2(0, 1))
    val h10 = valueAt(cell + IntV2(1, 0))
    val h11 = valueAt(cell + IntV2(1, 1))
    utils.math.bilerpUnit(h00, h01, h10, h11, unit.x, unit.y)
  }
}