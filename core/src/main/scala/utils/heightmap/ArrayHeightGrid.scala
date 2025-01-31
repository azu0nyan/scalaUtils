package utils.heightmap

import utils.datastructures.IntV2
import utils.math.Scalar

class ArrayHeightGrid(from:HeightGrid) extends HeightGrid {
  /** Grid size, indices will be from (0, 0) to resolution - (1, 1) */
  override val resolution: IntV2 = from.resolution

  val array:Array[Scalar] = Array.ofDim(valuesCount)
  for(i <- 0 until resolution.area){
    array(i) = from.valueAt(fromFlatIndex(i))
  }

  /** do not call directly, use "valueAt" for safety check */
  override def valueAtUnsafe(pos: IntV2): Scalar = array(toFlatIndex(pos))

  def setValue(pos:IntV2, newValue:Scalar):Unit = {
    if(validIndex(pos)){
      array(toFlatIndex(pos)) = newValue
    }
  }

  def modifyValue(pos:IntV2, delta:Scalar):Unit = {
    if(validIndex(pos)){
      array(toFlatIndex(pos)) += delta
    }
  }

}
