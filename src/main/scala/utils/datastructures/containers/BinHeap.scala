package utils.datastructures.containers

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** min-bin heap augmented with hash-table for fast updates */
class BinHeap[T](implicit o: Ordering[T]) {

  private val data: ArrayBuffer[T] = new ArrayBuffer[T]()


  private val elToId: mutable.Map[T, Int] = mutable.Map()

  @inline private def swap(f: Int, s: Int): Unit = {
    val df = data(f)
    val ds = data(s)
    data(f) = ds
    data(s) = df
    elToId(df) = s
    elToId(ds) = f
  }

  @inline private final def parent(i: Int): Int = (i - 1) >> 1
  @inline private final def leftChild(i: Int): Int = (i >> 1) + 1
  @inline private final def rightChild(i: Int): Int = (i >> 1) + 2

  def add(el: T): this.type = {
    data += el
    val id = data.size - 1
    elToId(el) = id
    siftUp(id)
    this
  }

  @inline private def siftUp(_id: Int): Unit = {
    var id = _id
    //while parent greater than child
    while (id > 1 && o.gt(data(parent(id)), data(id))) {
      swap(id, parent(id))
      id = parent(id)
    }
  }

  /** move element down tree */
  @inline private def siftDown(id: Int): Unit = {
    var cur = id
    var min = cur
    do {
      var lc = leftChild(id)
      var rc = rightChild(id)
      if (lc < data.size && o.lt(data(lc), data(min))) min = lc
      if (rc < data.size && o.lt(data(rc), data(min))) min = rc
      if (min != cur) {
        swap(min, id)
        cur = min
      }
    } while (min != cur)
  }

  def fillFrom(els: IndexedSeq[T]): this.type = {
    data.clearAndShrink(els.size)
    elToId.clear()
    for (i <- els.indices) {
      data += els(i)
      elToId += els(i) -> i
    }
    this
  }

  def remove(el: T): this.type = {
    val id = elToId(el)
    elToId -= el
    data(id) = data(data.size - 1)
    data.remove(data.size - 1)
    elToId(data(id)) = id
    this
  }

  def poll(): T = {
    val res = data(0)
    data(0) = data(data.size - 1)
    data.remove(data.size - 1)
    elToId -= res
    elToId(data(0)) = 0
    siftDown(0)
    res
  }


  def onOrderingChangedFor(el: T): this.type = {
    val id = elToId(el)
    if (id != 0 && o.lt(el, data(parent(id)))) siftUp(id)
    else siftDown(id)
    this
  }


  def peek(): Option[T] = Option.when(data.nonEmpty)(data(0))

  def contains(el: T): Boolean = elToId.contains(el)


}
