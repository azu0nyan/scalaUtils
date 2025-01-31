package utils.heightmap

import utils.datastructures.IntV2
import utils.math.*

case class HeightmapToHeightGrid(heightmap: Heightmap, resSamples:IntV2) extends TiledHeightmap {
  override def resolution: IntV2 = resSamples

  override def valueAtUnsafe(pos: IntV2): Scalar = heightmap.heightAt(pos.toV2)
}
