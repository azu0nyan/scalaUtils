package utils.datastructures

class ImmutableGrid[T](flatVals:IndexedSeq[T], res:IntV2) extends Grid[T] {
  override def resolution: IntV2 = res

  override def valueAtUnsafe(pos: IntV2): T = flatVals(toFlatIndex(pos))
}
