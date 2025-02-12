package utils.datastructures

import utils.heightmap.GridOps
import utils.math.*
import utils.math.planar.V2

/** Grid with values at integer points, use indices or values to iterate over */
trait Grid[T] {

  /** Grid size, indices will be from (0, 0) to resolution - (1, 1) */
  def resolution: IntV2
  @inline def apply(pos:IntV2):T = valueAt(pos)

  /** pos will be clamped to  [(0, 0), resolution - (1, 1)] */
  @inline def valueAt(pos: IntV2): T = valueAtUnsafe(clampResolutionIndices(pos))

  @inline def apply(flat:Int):T = valueAt(fromFlatIndex(flat))

  @inline def valueAt(flat:Int):T = valueAt(fromFlatIndex(flat))

  @inline def validIndex(pos: IntV2): Boolean = pos.x >= 0 && pos.y >= 0 && pos. x < resolution.x && pos.y < resolution.y

  /** do not call directly, use "valueAt" for safety check */
  def valueAtUnsafe(pos: IntV2): T

  def valueAtUnsafeXY(x:Int, y:Int): T = valueAtUnsafe(IntV2(x, y))

  /** clamp to  [(0, 0), resolution - (1, 1)] */
  final def clampResolutionIndices(divIndex: IntV2): IntV2 = IntV2(math.min(resolution.i - 1, math.max(divIndex.i, 0)), math.min(resolution.j - 1, math.max(divIndex.j, 0)))

  /** for storing grid in 1D array */
  @inline def toFlatIndex(index: IntV2): Int = CircullarOps.circullarIndex((index.i * resolution.j) + index.j, valuesCount)

  @inline def fromFlatIndex(index: Int): IntV2 = IntV2(index / resolution.j, index % resolution.j)

  @inline def valuesCount: Int = resolution.i * resolution.j

  def indicesAndValues: Iterator[(IntV2, T)] = for (i <- 0 until resolution.i iterator; j <- 0 until resolution.j iterator) yield (IntV2(i, j), valueAtUnsafeXY(i, j))
  def indices: Iterator[IntV2] = for (i <- 0 until resolution.i iterator; j <- 0 until resolution.j iterator) yield IntV2(i, j)
  def flatIndices: Iterator[Int] = for (i <- 0 until resolution.area iterator) yield i

  def values: Iterator[T] = indices.iterator.map(i => valueAt(i))

  def neighboursXY(i: IntV2): Seq[IntV2] =
    Seq(
      Option.when(i.x > 0)(IntV2(i.x - 1, i.y)),
      Option.when(i.y > 0)(IntV2(i.x, i.y - 1)),
      Option.when(i.x < resolution.x - 1)(IntV2(i.x + 1, i.y)),
      Option.when(i.y < resolution.y - 1)(IntV2(i.x, i.y + 1)),
    ).flatten

  def neighboursDiag(i: IntV2): Seq[IntV2] =
    Seq(
      Option.when(i.x > 0 && i.y > 0)(IntV2(i.x - 1, i.y - 1)),
      Option.when(i.x > 0 && i.y < resolution.y - 1)(IntV2(i.x - 1, i.y + 1)),
      Option.when(i.x < resolution.x - 1 && i.y > 0)(IntV2(i.x + 1, i.y - 1)),
      Option.when(i.x < resolution.x - 1 && i.y < resolution.y - 1)(IntV2(i.x + 1, i.y + 1)),
      Option.when(i.x > 0)(IntV2(i.x - 1, i.y)),
      Option.when(i.y > 0)(IntV2(i.x, i.y - 1)),
      Option.when(i.x < resolution.x - 1)(IntV2(i.x + 1, i.y)),
      Option.when(i.y < resolution.y - 1)(IntV2(i.x, i.y + 1)),
    ).flatten

  def toArrayGrid:ArrayGrid[T] = new ArrayGrid[T](values.toArray, resolution)

  def interpolateFromCellCenters(mix:(T, T, Scalar) => T, pos:V2):T = {
    GridOps.interpolateFromCellCenters((x, y) => apply(x, y), resolution.x, resolution.y, mix, pos.x, pos.y)
  }
}
