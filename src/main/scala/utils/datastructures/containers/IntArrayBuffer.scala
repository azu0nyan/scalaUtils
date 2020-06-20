package utils.datastructures.containers

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
/** grows x2, without boxing */
class IntArrayBuffer(val initialCapacity:Int = 8) extends Seq[Int]{

  def addLast(elem:Int):Unit = {
    if(filled  >= data.length){
      data = Array.copyOf(data, data.length * 2)
    }
    data(filled) = elem
    filled += 1
  }

  def removeLast():Int = {
    val res = data(filled - 1)
    filled -= 1
    if(filled >= initialCapacity * 2 && filled < data.length / 3){ //if 2/3 empty
      data = Array.copyOf(data, data.length / 2)
    }
    res
  }

  private var filled:Int  = 0

  var data:Array[Int] = Array.ofDim(initialCapacity)

  def clear():Unit = {
    filled = 0
    if(data.length > initialCapacity * 2) {
      data =  Array.ofDim(initialCapacity)
    }
  }

  def update(idx:  Int, elem:  Int): Unit = data(idx) = elem

  override def apply(i:  Int): Int = data(i)

  override   def length: Int = filled

  override   def iterator: Iterator[Int] = for(i <- 0 until filled iterator) yield data(i)

}
