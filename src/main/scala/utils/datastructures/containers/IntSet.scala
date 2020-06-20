package utils.datastructures.containers

import scala.collection.mutable

/** fast set for integer with 2^bucketsPow^  buckets (65536 by default) without boxing*/
class IntSet(val bucketsPow: Int = 16) extends mutable.Set[Int] {

  private val bucketMask: Int = (for (i <- 0 until bucketsPow) yield (1 << i)).reduce(_ | _)

  private val bucketsCount: Int = 1 << bucketsPow
  private val buckets: Array[Array[Int]] = Array.fill[Array[Int]](bucketsCount)(null)

  @inline private def bucketId(elem: Int): Int = elem & bucketMask

  override def contains(elem: Int): Boolean = {
    val bucket = buckets(bucketId(elem))
    return bucket != null && bucket.contains(elem)
  }

  override def clear(): Unit = {
    buckets.indices.foreach(buckets(_) = null)
  }

  override def iterator: _root_.scala.collection.Iterator[Int] = new Iterator[Int] {
    var currentBucket = 0
    var inCurrentBucket: Int = -1

    def skipToNext(): Unit = {
      if (buckets(currentBucket) == null) inCurrentBucket = -1
      else if (buckets(currentBucket).length - 1 == inCurrentBucket) { //если на прошлом шагу мы дошли до конца бакета
        currentBucket += 1
        inCurrentBucket = -1
      }
      while (currentBucket < bucketsCount && buckets(currentBucket) == null) currentBucket += 1// скип до следующего непустого(только если текущий был пуст или закончился)
      inCurrentBucket += 1 //перехрдим к первому/следубщему элементу
    }
    skipToNext()

    override def hasNext: Boolean = currentBucket < bucketsCount

    override def next(): Int = {
      val res = buckets(currentBucket)(inCurrentBucket)
      skipToNext()
      res
    }
  }
  override def subtractOne(elem: Int): IntSet.this.type = {
    val b = bucketId(elem)
    if(buckets(b) != null){
      if(buckets(b).length == 1 && buckets(b)(0) == elem) {
        buckets(b) = null
      } else {
        buckets(b) = buckets(b).filter(_ != elem)
      }
    }
    this
  }
  override def addOne(elem: Int): IntSet.this.type = {
    if(!contains(elem)) {
      val b = bucketId(elem)
      if(buckets(b) == null){
        buckets(b) = Array(elem)
      } else {
        buckets(b) = buckets(b).appended(elem)
      }
    }
    this
  }
}
