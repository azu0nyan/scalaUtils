package utils.datastructures.containers

import java.util.NoSuchElementException

import utils.datastructures.containers.DoubleLinkedList.Node

import scala.collection.mutable


object DoubleLinkedList {


  implicit class TransitionHelper[T](val v: (Option[Node[T]], Option[Node[T]], T => Node[T])) extends AnyVal {
    def before: Option[Node[T]] = v._1

    def after: Option[Node[T]] = v._2

    def insert: T => Node[T] = v._3
  }

  class Node[T](
                 var value: T,
                 var next: Node[T],
                 var prev: Node[T],
                 var container: DoubleLinkedList[T]
               ) {

    def removeMe(): T = {
      if (container.firstNode == this) {
        container.removeFirst()
      } else if (container.lastNode == this) {
        container.removeLast()
      } else { //next prev exists
        prev.next = next
        next.prev = prev
        container._size -= 1
      }
      return value
    }

    def insertBefore(el: T): Node[T] = {
      if (container.firstNode == this) {
        return container.addFirst(el)
      } else {
        val res = new Node[T](el, this, prev, container)
        if (prev != null) prev.next = res
        prev = res
        container._size += 1
        return res
      }
    }

    def insertAfter(el: T): Node[T] = {
      if (container.lastNode == this) {
        return container.addLast(el)
      } else {
        val res = new Node[T](el, next, this, container)
        if (next != null) next.prev = res
        next = res
        container._size += 1
        return res
      }
    }
  }

  class DoubleLinkedList[T] {
    var _size: Int = 0

    var firstNode: Node[T] = null

    var lastNode: Node[T] = null

    def first: T = firstNode.value

    def second: T = firstNode.next.value

    def third: T = firstNode.next.next.value

    def firstOption: Option[T] = Option(firstNode.value)

    def last: T = lastNode.value

    def secondLast: T = lastNode.prev.value

    def thirdLast: T = lastNode.prev.prev.value

    def lastOption: Option[T] = Option(lastNode.value)

    def removeFirst(): T = {
      if (firstNode != null) {
        _size -= 1
        val res = firstNode.value
        if (lastNode == firstNode) { //size == 1
          firstNode = null
          lastNode = null
        } else {
          firstNode = firstNode.next
          firstNode.prev = null
        }
        return res
      } else {
        throw new NoSuchElementException
      }
    }

    def removeLast(): T = {
      if (lastNode != null) {
        _size -= 1
        val res = lastNode.value
        if (lastNode == firstNode) {
          firstNode = null
          lastNode = null
        } else {
          lastNode = lastNode.prev
          lastNode.next = null
        }
        return res
      } else {
        throw new NoSuchElementException()
      }
    }

    def addFirst(el: T): Node[T] = {
      firstNode = new Node(el, firstNode, null, this)
      if (firstNode.next != null) {
        firstNode.next.prev = firstNode
      }
      if (lastNode == null) {
        lastNode = firstNode
      }
      _size += 1
      return firstNode
    }

    def addLast(el: T): Node[T] = {
      lastNode = new Node(el, null, lastNode, this)
      if (lastNode.prev != null) {
        lastNode.prev.next = lastNode
      }
      if (firstNode == null) {
        firstNode = lastNode
      }
      _size += 1
      return lastNode
    }

    /**
      * @param pred returns true if we want insert before given node
      *             if no node found inserts at the end
      */
    def insertBefore(el: T, pred: T => Boolean): Node[T] = {
      nodesIterator.find(v => pred(v.value)) match {
        case Some(node) => node.insertBefore(el)
        case None => addLast(el)
      }
    }

    /**
      * @param pred returns true if we want insert after given node
      *             if no node found inserts at the end
      */
    def insertAfter(el: T, pred: T => Boolean): Unit = {
      nodesIterator.find(v => pred(v.value)) match {
        case Some(node) => node.insertAfter(el)
        case None => addLast(el)
      }
    }

    /*return nodes before and after the  condition changed from false to true, and function to insert between
    * use function once
    * */

    def findTransition(pred: T => Boolean): (Option[Node[T]], Option[Node[T]], T => Node[T]) = {
      if (_size == 0) {
        return (None, None, t => addFirst(t))
      } else {
        var prev: Node[T] = null
        var current: Node[T] = firstNode
        while (current != null && !pred(current.value)) {
          prev = current
          current = current.next
        }
        return (Option(prev), Option(current), new (T => Node[T]) {
          var pr: Node[T] = prev
          var ne: Node[T] = current

          override def apply(el: T): Node[T] = {
            if (pr != null) {
              pr = pr.insertAfter(el)
              return pr
            } else if (ne != null) {
              ne = ne.insertBefore(el)
              return ne
            } else {
              return addFirst(el)
            }
          }
        })
      }
    }

    def reverseInPlace(): DoubleLinkedList.this.type = {
      if (_size > 1) {
        var current = firstNode
        while (current != null) {
          val next = current.next

          val tmp = current.next
          current.next = current.prev
          current.prev = tmp

          current = next
        }
        val tmp = firstNode
        firstNode = lastNode
        lastNode = tmp
      }
      return this
    }

    /**
      * @param other linked list will be invalidated, do not use after
      * @return
      */
    def concatWithUnsafe(other: DoubleLinkedList[T]): DoubleLinkedList.this.type = {
      if (other._size > 0) {
        _size += other._size

        //Link firs and last
        lastNode.next = other.firstNode
        other.firstNode.prev = lastNode
        //replace last
        lastNode = other.lastNode
        other.nodesIterator.foreach(_.container = this)
      }
      return this
    }

    def getNode(idx: Int): Node[T] = nodesIterator.drop(idx).next()

    def nodesIterator: Iterator[Node[T]] = new Iterator[Node[T]] {
      private var current: Node[T] = firstNode

      override def hasNext: Boolean = current != null

      override def next(): Node[T] = {
        val res = current
        current = current.next
        return res
      }
    }

    def length: Int = _size

    def size: Int = _size

    def get(i: Int): T = getNode(i).value

    /**
      * To immutable seq interface without reallocation,
      * do not update list after
      *
      * @return
      */
    def bakeToSeq: Seq[T] = new Seq[T] {
      override def apply(i: Int): T = get(i)

      override def length: Int = _size

      override def iterator: Iterator[T] = DoubleLinkedList.this.iterator
    }

    def iterator: Iterator[T] = new Iterator[T] {
      private var current: Node[T] = firstNode

      override def hasNext: Boolean = current != null

      override def next(): T = {
        val res = current.value
        current = current.next
        return res
      }
    }


    def nonEmpty:Boolean = size > 0

    def empty:Boolean = size == 0

    /**
      * DDL to scala buffer wrapper
      */
    val toBuffer: mutable.Buffer[T] = new mutable.Buffer[T] {

      override def update(idx: Int, elem: T): Unit = DoubleLinkedList.this.nodesIterator.drop(idx).next().value = elem

      override def apply(i: Int): T = iterator.drop(i).next

      override def length: Int = DoubleLinkedList.this._size


      override def iterator: Iterator[T] = DoubleLinkedList.this.iterator

      override def prepend(elem: T): this.type = {
        DoubleLinkedList.this.addFirst(elem)
        return this
      }

      override def insert(idx: Int, elem: T): Unit = DoubleLinkedList.this.getNode(idx).insertAfter(elem)

      override def insertAll(idx: Int, elems: IterableOnce[T]): Unit = {
        var n = DoubleLinkedList.this.getNode(idx)
        elems.iterator.foreach(e => n = n.insertAfter(e))
      }

      override def remove(idx: Int): T = DoubleLinkedList.this.getNode(idx).removeMe()

      override def remove(idx: Int, count: Int): Unit = {
        var el = DoubleLinkedList.this.getNode(idx)
        for (_ <- 0 until count) {
          el.removeMe()
          el = el.next
        }
      }

      override def patchInPlace(from: Int, patch: IterableOnce[T], replaced: Int): this.type = {
        var node = DoubleLinkedList.this.getNode(from)
        val pIter = patch.iterator
        var leftToReplace = replaced
        while (node != null && pIter.hasNext && leftToReplace > 0) {
          node.value = pIter.next()
          leftToReplace -= 1
        }
        return this
      }

      override def addOne(elem: T): this.type = prepend(elem)

      override def clear(): Unit = {
        DoubleLinkedList.this._size = 0
        DoubleLinkedList.this.firstNode = null
        DoubleLinkedList.this.lastNode = null

      }
    }



  }

}






