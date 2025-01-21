package utils.math.planar.algo.straightSkeleton.helpers

import utils.datastructures.containers.map.impl.MutableMultiMap
import utils.math.space.V3

import scala.collection.mutable

class GraphMap() {
  val map: MutableMultiMap[V3, V3, mutable.ArrayBuffer] = new MutableMultiMap[V3, V3, mutable.ArrayBuffer]
  
  def add(a: V3, b: V3) = {
    map.put(a, b)
    map.put(b, a)
  }
  
  
}

