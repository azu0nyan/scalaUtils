package utils.datastructures

class ImmutableGrid[T](vals:Seq[T], res:IntV2) extends Grid[T] {
  override def resolution: IntV2 = res

  override def valueAtUnsafe(pos: IntV2): T = vals(toFlatIndex(pos))
}
