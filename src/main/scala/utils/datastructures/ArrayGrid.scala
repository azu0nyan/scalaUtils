package utils.datastructures

import utils.heightmap.HeightGrid
import utils.math.Scalar

class ArrayGrid[T](array:Array[Any], override val resolution:IntV2) extends Grid [T]{
  /** do not call directly, use "valueAt" for safety check */
  override def valueAtUnsafe(pos: IntV2): T = array(toFlatIndex(pos)).asInstanceOf[T]

  def setValue(pos:IntV2, newValue:T):Unit = {
    if(validIndex(pos)){
      array(toFlatIndex(pos)) = newValue.asInstanceOf[Any]
    }
  }

  def setValue(flat:Int, newValue:T):Unit = {
      array(flat) = newValue.asInstanceOf[Any]
  }

  @inline override def apply(flat:Int):T = array(flat).asInstanceOf[T]

  @inline override def valueAt(flat:Int):T = array(flat).asInstanceOf[T]


}
