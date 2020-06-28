package utils.heightmap

import utils.datastructures.{Grid, IntV2}
import utils.math.space.{Plane, V3}
import utils.math._

trait HeightGrid extends Grid[Scalar] {

  final val heightAt: IntV2 => Scalar = valueAt
  //cells(insides of grid)
  final def cells: IntV2 = IntV2(resolution.i - 1, resolution.j - 1)

  final def clampCellIndices(cellIndex: IntV2): IntV2 = IntV2(math.min(cells.i - 1, math.max(cellIndex.i, 0)), math.min(cells.j - 1, math.max(cellIndex.j, 0)))

  def cellIndices: Seq[IntV2] = for (i <- 0 until cells.i; j <- 0 until cells.j) yield IntV2(i, j)
  //values

  @inline def toFlatHeightIndex(index: IntV2): Int = toFlatIndex(index)

  def vertices(cordPlane: Plane): IterableOnce[V3] = indices.iterator.map(i => cordPlane.toWorldCords(i.toV2, valueAt(i)))

  def maxHeight: Scalar = values.iterator.max

  def minHeight: Scalar = values.iterator.min

  //transforms
  def scaled(scale: Int): HeightGrid = HeightGridScaled(this, scale)

  //  def *(scale:Int):Tilemap = scaled(scale)

  def toHeightmap: Heightmap = HeightGridToHeightmap(this)

  def bakeToArray: ArrayHeightGrid = new ArrayHeightGrid(this)
  def bakeToPow2Array: Pow2ArrayHeightGrid = new Pow2ArrayHeightGrid(this)

  /**!!don't doing safety checks*/
  def viewSubgrid(leftTop:IntV2, size:IntV2):HeightGrid = new HeightGrid {
    /** Grid size, indices will be from (0, 0) to resolution - (1, 1) */
    override def resolution: IntV2 = size
    /** do not call directly, use "valueAt" for safety check */
    override def valueAtUnsafe(pos: IntV2): Scalar = HeightGrid.this.valueAt(pos + leftTop)
  }
}