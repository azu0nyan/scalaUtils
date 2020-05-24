package utils.datastructures

import scala.collection.mutable.ArrayBuffer

class MutableGrid[T](res:IntV2, defValue:T = null) extends Grid[T] {

  val array:ArrayBuffer[T] = new ArrayBuffer[T]()
  array ++= (for (i <- 0 until valuesCount) yield defValue)

  override def resolution: IntV2 = res

  override def valueAtUnsafe(pos: IntV2): T = array(toFlatIndex(pos))

  def setValue(value:T, pos:IntV2): Unit = array(toFlatIndex(pos)) = value
}
