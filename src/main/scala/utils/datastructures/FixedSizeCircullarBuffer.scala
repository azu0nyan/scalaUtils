package utils.datastructures

import scala.collection.{IndexedSeqView, Seq}
import scala.collection.mutable.ArrayBuffer

class FixedSizeCircullarBuffer[A](maxSize: Int)  {

  val data:ArrayBuffer[A] = new ArrayBuffer[A](maxSize)

  private var current = 0

  private def lastId:Int = utils.math.circullarIndex(current - 1, data.length)
  def lastAdded:Option[A] = Option.when(lastId < data.length && lastId >= 0)(data(lastId))

  def addNext(el: A): FixedSizeCircullarBuffer[A] = {
    if(current>= maxSize) {
      current -= maxSize
    }
    if(current < data.length ){
      data.update(current, el)
    } else {
      data.addOne(el)
    }
    current += 1
    this
  }

}
