package utils.datastructures.containers

import utils.math.Scalar

import scala.collection.mutable

class DoubleBinHeap[T] {
  
  val map = new mutable.HashMap[T, Double]()  
  val binHeap: BinHeap[T] = new BinHeap[T]()(Ordering.by(map))
  
  def set(t: T, value: Scalar): Unit = {
    if(map.contains(t)){
      map += t -> value
      binHeap.onOrderingChangedFor(t)
    } else {
      map += t -> value
      binHeap.add(t)
    }
  }
  
  def poll():T  = {
    val res = binHeap.poll()
    map -= res 
    res 
  } 
  
  def isEmpty: Boolean = binHeap.empty

}
