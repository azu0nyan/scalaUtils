package utils.datastructures

/** Grid with values at integer points, use indices or values to iterate over*/
trait Grid[T] {

  /**Grid size, indices will be from (0, 0) to resolution - (1, 1) */
  def resolution: IntV2

  /**pos will be clamped to  [(0, 0), resolution - (1, 1)] */
  @inline def valueAt(pos: IntV2): T = valueAtUnsafe(clampResolutionIndices(pos))

  /**do not call directly, use "valueAt" for safety check */
  def valueAtUnsafe(pos: IntV2): T

  /** clamp to  [(0, 0), resolution - (1, 1)] */
  final def clampResolutionIndices(divIndex: IntV2):IntV2 = IntV2(math.min(resolution.i - 1, math.max(divIndex.i, 0)), math.min(resolution.j - 1, math.max(divIndex.j, 0)))

  /**for storing grid in 1D array*/
  @inline def toFlatIndex(index: IntV2): Int = CircullarOps.circullarIndex((index.i * resolution.j) + index.j, valuesCount)

  @inline def fromFlatIndex(index: Int): IntV2 = IntV2(index / resolution.j, index % resolution.j)

  @inline def valuesCount: Int = resolution.i * resolution.j

  def indices: IterableOnce[IntV2] = for(i <- 0 until resolution.i; j<- 0 until resolution.j) yield IntV2(i, j)

  def values:IterableOnce[T] = indices.iterator.map(i => valueAt(i))
}
