package utils.heightmap

import utils.math.*
import utils.math.planar.V2

case class HeightGridToHeightmap(tilemap: HeightGrid)extends Heightmap {
  override def heightAt(pos: V2): Scalar = tilemap.valueAt(pos.clamp(V2.ZERO, tilemap.resolution.toV2).toIntV2)
}
