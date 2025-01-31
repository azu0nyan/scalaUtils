package utils.datastructures.containers

/** min-bin heap augmented with hash-table for fast updates */
class IntBinHeap(implicit lt: (Int,Int) => Boolean) {

  def gt(x:Int, y:Int):Boolean = x != y && lt(y, x)

  def size:Int = data.size

  private val data = new IntArrayBuffer

  private val elToId: IntToIntBucketMap = new IntToIntBucketMap()

  @inline private def swap(f: Int, s: Int): Unit = {
    val df = data(f)
    val ds = data(s)
    data(f) = ds
    data(s) = df
    elToId.update(df, s)
    elToId.update(ds, f)
  }

  @inline def empty: Boolean = data.isEmpty
  @inline def nonEmpty: Boolean = data.nonEmpty

  @inline private final def parent(i: Int): Int = (i - 1) >> 1
  @inline private final def leftChild(i: Int): Int = (i << 1) + 1
  @inline private final def rightChild(i: Int): Int = (i << 1) + 2


  @inline private def siftUp(_id: Int): Unit = {
    var id = _id
    //while parent greater than child
    while (id >= 1 && gt(data(parent(id)), data(id))) {
      swap(id, parent(id))
      id = parent(id)
    }
  }

  /** move element down tree */
  @inline private def siftDown(id: Int): Unit = {
    var cur = id
    var cont = true
    while (cont) {
      var min = cur
      var lc = leftChild(cur)
      var rc = rightChild(cur)
      if (lc < data.size && lt(data(lc), data(min))) min = lc
      if (rc < data.size && lt(data(rc), data(min))) min = rc
      if (min != cur) {
        swap(min, cur)
        cur = min
      } else cont = false
    }
  }

  def fillFrom(els: IndexedSeq[Int]): this.type = {
    data.clear()
    elToId.clear()
    for (i <- els.indices) {
      data.addLast(els(i))
      elToId += els(i) -> i
    }
    this
  }

  def +=(el: Int): this.type = add(el)

  def -=(el: Int): this.type = remove(el)

  def add(el: Int): this.type = {
    data.addLast(el)
    val id = data.size - 1
    elToId(el) = id
    siftUp(id)
    this
  }

  def remove(el: Int): this.type = {
    val id = elToId(el)
    elToId -= el
    data(id) = data(data.size - 1)
    data.removeLast()
    elToId(data(id)) = id
    this
  }

  def poll(): Int = {
    val res = data(0)
    elToId -= res
    if (data.size > 1) {
      //move last forward
      data(0) = data(data.size - 1)
      elToId(data(0)) = 0
      data.removeLast()
      siftDown(0)
    } else {
      data.removeLast()
    }

    res

  }


  def onOrderingChangedFor(el: Int): this.type = {
    val id = elToId(el)
    if (id != 0 && lt(el, data(parent(id)))) siftUp(id)
    else siftDown(id)
    this
  }


  def update(oldEl: Int, newEl: Int): this.type = {
    val id = elToId(oldEl)
    elToId -= oldEl
    data(id) = newEl
    elToId(newEl) = id
    onOrderingChangedFor(newEl)
  }


  def peek(): Option[Int] = Option.when(data.nonEmpty)(data(0))

  def contains(el: Int): Boolean = elToId.contains(el)

  def takeAllIterator(): Iterator[Int] = new Iterator[Int] {
    override def hasNext: Boolean = !empty
    override def next(): Int = poll()
  }

}

