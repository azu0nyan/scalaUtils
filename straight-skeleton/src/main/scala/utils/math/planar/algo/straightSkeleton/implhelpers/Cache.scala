package utils.math.planar.algo.straightSkeleton.implhelpers

import scala.collection.mutable


abstract class Cache[I, O] {
  var cache = new mutable.LinkedHashMap[I, O]

  def get(in: I) =
    cache.getOrElseUpdate(in, create(in))

  def create(i: I): O

  def put(start: I, start0: O): Unit =
    cache.put(start, start0)
  
  def clear(): Unit =
    cache.clear()
}

