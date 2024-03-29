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
  def pollWithValue():(T, Double) = {
    val res1 = binHeap.poll()
    val res2 = map(res1)
    map -= res1
    (res1, res2)
  }

  def poll():T  = {
    val res = binHeap.poll()
    map -= res 
    res 
  } 
  
  def isEmpty: Boolean = binHeap.empty

}
