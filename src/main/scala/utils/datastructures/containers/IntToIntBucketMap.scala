package utils.datastructures.containers

import scala.collection.mutable

/** Int to Int map without boxing(if you use methods defined here) */
class IntToIntBucketMap(bucketsPow: Int = 16)extends IntBucketMap[Int](bucketsPow)

class IntBucketMap[@specialized T](val bucketsPow: Int = 16) extends mutable.Map[Int, T] {

  override def size: Int = _size
  var _size: Int = 0

  class Node(val key: Int, var value: T)

  private val bucketMask: Int = (for (i <- 0 until bucketsPow) yield (1 << i)).reduce(_ | _)

  private val bucketsCount: Int = 1 << bucketsPow


  private val buckets: Array[Array[Node]] = Array.ofDim(bucketsCount) //Array.fill[Array[(Int, Int)]](bucketsCount)(null)

  @inline private def bucketId(elem: Int): Int = elem & bucketMask

  override def apply(key: Int): T = get(key).get

  override def get(key: Int): Option[T] = {
    val bucket = buckets(bucketId(key))
    if (bucket != null) bucket.find(_.key == key).map(_.value)
    else None
  }

  override def iterator: Iterator[(Int, T)] = new Iterator[(Int, T)] {
    var currentBucket = 0
    var inCurrentBucket: Int = -1

    def skipToNext(): Unit = {
      if (buckets(currentBucket) == null) inCurrentBucket = -1
      else if (buckets(currentBucket).length - 1 == inCurrentBucket) { //если на прошлом шагу мы дошли до конца бакета
        currentBucket += 1
        inCurrentBucket = -1
      }
      while (currentBucket < bucketsCount && buckets(currentBucket) == null) currentBucket += 1 // скип до следующего непустого(только если текущий был пуст или закончился)
      inCurrentBucket += 1 //перехрдим к первому/следубщему элементу
    }
    skipToNext()

    override def hasNext: Boolean = currentBucket < bucketsCount
    override def next(): (Int, T) = {
      val res = buckets(currentBucket)(inCurrentBucket)
      skipToNext()
      (res.key, res.value)
    }
  }


  override def addOne(elem: (Int, T)): IntBucketMap.this.type = {
    if (!contains(elem._1)) {
      _size += 1
      val b = bucketId(elem._1)
      if (buckets(b) == null) {
        buckets(b) = Array(new Node(elem._1, elem._2))
      } else {
        buckets(b) = buckets(b).appended(new Node(elem._1, elem._2))
      }
    }
    this
  }
  override def subtractOne(elem: Int): IntBucketMap.this.type = {
    val b = bucketId(elem)
    if (buckets(b) != null) {
      if (buckets(b).length == 1 && buckets(b)(0).key == elem) {
        buckets(b) = null
        _size -= 1
      } else {
        val before = buckets(b).length
        buckets(b) = buckets(b).filter(_.key != elem)
        if (before > buckets(b).length) _size -= 1
      }
    }
    this
  }

  override def update(key: Int, value: T): Unit = {
    val b = bucketId(key)
    val bucket: Array[Node] = buckets(b)
    if (bucket != null) {
      var i = 0
      var cont = true
      while (cont && i < bucket.length) {
        if (bucket(i).key == key) {
          bucket(i).value = value
          cont = false
        }
        i += 1
      }

    }
  }

  override def clear(): Unit = {
    buckets.indices.foreach(buckets(_) = null)
    _size = 0
  }


}
