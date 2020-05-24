package utils.heightmap

import utils.math.planar.V2
import utils.math._

case class TilemapToHeightmap (tilemap: Tilemap)extends Heightmap {
  override def heightAt(pos: V2): Scalar = tilemap.valueAt(pos.clamp(V2.ZERO, tilemap.resolution.toV2).toIntV2)
}
