package utils.datastructures.containers

import scala.collection.mutable


object ThreadedAVLTree {
  type OptNode[T] = Option[TreeNode[T]]

  /**
    * @param value         value stored in node
    * @param balanceFactor H(L) - H(R)
    * @param parent        node None if this node is root
    * @param left          subtree
    * @param right         subtree
    * @param next          node ordered by 'value'
    * @param prev          node ordered by 'value
    * @tparam T type
    */
  class TreeNode[T](
                     var value: T,
                     var balanceFactor: Int = 0,
                     var parent: OptNode[T] = None,
                     var left: OptNode[T] = None,
                     var right: OptNode[T] = None,
                     var next: OptNode[T] = None,
                     var prev: OptNode[T] = None,
                   ) {
    override def toString = s"TreeNode(value=$value, balanceFactor=$balanceFactor)"
  }

  class ThreadedAVLTree[T](implicit o: Ordering[T]) {

    private var _size: Int = 0

    def size: Int = _size

    var root: OptNode[T] = None

    //    @inline def goLeft(nodeValue: T, value: T): Int = o.compare(nodeValue, value)

    def add(toAdd: T): TreeNode[T] = {
      if (root.isEmpty) {
        root = Some(new TreeNode[T](toAdd, 0))
        _size += 1
        return root.get
      } else {
        var parent: TreeNode[T] = root.get
        var current: OptNode[T] = root
        var direction: Int = 0
        while (current.isDefined) { //while we on existing node go deeper
          parent = current.get
          direction = o.compare(toAdd, parent.value) // -1 go left, 1 go right
          if (direction == 0) { //parent already holds "to left"
            return parent
          }
          //go deeper
          current = if (direction == -1) parent.left else parent.right
        }
        val newNode = new TreeNode[T](value = toAdd, balanceFactor = 0, parent = Some(parent))
        if (direction == -1) {
          parent.left = Some(newNode)
        } else {
          parent.right = Some(newNode)
        }
        newNode.next = findClothestGreaterParent(newNode)
        newNode.next.foreach(_.prev = Some(newNode))
        newNode.prev = findClothestLesserParent(newNode)
        newNode.prev.foreach(_.next = Some(newNode))

        traverseUpAndBalance(parent, if (direction == -1) 1 else -1) //-direction
        _size += 1
        return newNode
      }
    }

    /*up traversal helper*/
    @inline private def cameFromLeft(parent: TreeNode[T], sibling: TreeNode[T]): Boolean = parent.left.isDefined && parent.left.get == sibling

    /** min node in subtree, for finding replacement while deleting */
    def minNode(node: TreeNode[T]): TreeNode[T] = {
      var tmp = node
      while (tmp.left.isDefined) {
        tmp = tmp.left.get
      }
      return tmp
    }

    /** max node in subtree, for finding replacement while deleting */
    def maxNode(node: TreeNode[T]): TreeNode[T] = {
      var tmp = node
      while (tmp.right.isDefined) {
        tmp = tmp.right.get
      }
      return tmp
    }

    /** for finding next and prev nodes after leaf inserted */
    def findClothestLesserParent(node: TreeNode[T]): OptNode[T] = {
      var parentNode = node.parent
      var siblingNode = node
      while (parentNode.isDefined && cameFromLeft(parentNode.get, siblingNode)) {
        siblingNode = parentNode.get
        parentNode = siblingNode.parent
      }
      return parentNode
    }

    /** for finding next and prev nodes after leaf inserted */
    def findClothestGreaterParent(node: TreeNode[T]): OptNode[T] = {
      var parentNode = node.parent
      var siblingNode = node
      while (parentNode.isDefined && !cameFromLeft(parentNode.get, siblingNode)) {
        siblingNode = parentNode.get
        parentNode = siblingNode.parent
      }
      return parentNode
    }

    /*remove helper*/
    private def removeChildAndBalance(child: TreeNode[T], parent: TreeNode[T]): Unit =
      if (cameFromLeft(parent, child)) {
        parent.left = None
        traverseUpAndBalanceOnRemove(parent, -1)
      } else {
        parent.right = None
        traverseUpAndBalanceOnRemove(parent, 1)
      }

    /*remove helper*/
    private def replaceChildByGrandChildAndBalance(child: TreeNode[T], grandChild: TreeNode[T], parent: TreeNode[T]): Unit =
      if (cameFromLeft(parent, child)) {
        parent.left = Some(grandChild)
        grandChild.parent = Some(parent)
        traverseUpAndBalanceOnRemove(parent, -1)
      } else {
        parent.right = Some(grandChild)
        grandChild.parent = Some(parent)
        traverseUpAndBalanceOnRemove(parent, 1)
      }

    /*remove helper*/
    private def relinkNextPrevsAndCopyVal(from: TreeNode[T], to: TreeNode[T]): Unit = {
      to.value = from.value
      to.prev = from.prev
      from.prev.foreach(p => p.next = Some(to))
      to.next = from.next
      from.next.foreach(n => n.prev = Some(to))
    }


    def removeNode(node: TreeNode[T]): Unit = {
      //prev --> node --> next ==> prev --> next
      node.prev.foreach { p =>
        p.next = node.next //works right if node.next == None
      }
      node.next.foreach {
        n => n.prev = node.prev
      }
      //node we trying to delete
      var currentNode = node
      var deleted = false
      while (!deleted) {
        (currentNode.left, currentNode.right, currentNode.parent) match {
          case (None, None, None) =>
            root = None
            deleted = true
          case (Some(l), None, None) =>
            l.parent = None
            root = Some(l)
            deleted = true
          case (None, Some(r), None) =>
            r.parent = None
            root = Some(r)
            deleted = true
          case (None, None, Some(parent)) =>
            removeChildAndBalance(currentNode, parent)
            deleted = true
          case (Some(l), None, Some(parent)) =>
            replaceChildByGrandChildAndBalance(currentNode, l, parent)
            deleted = true
          case (None, Some(r), Some(parent)) =>
            replaceChildByGrandChildAndBalance(currentNode, r, parent)
            deleted = true
          case (Some(l), Some(r), _) =>
            val replacement = if (currentNode.balanceFactor >= 0) { //left deeper
              maxNode(l) //rightmost in left subtree
            } else {
              minNode(r) //leftmost in right subtree
            }
            relinkNextPrevsAndCopyVal(replacement, currentNode)
            currentNode = replacement
        }
      }
      _size -= 1
    }

    def removeByPredicate(pred: T => Int): Option[T] = findByPredicate(pred) match {
      case Some(node) =>
        val res = node.value
        removeNode(node)
        return  Some(res)
      case None => None
    }

    def remove(toRemove: T): Boolean = findNode(toRemove) match {
      case Some(node) =>
        removeNode(node)
        true
      case None => false
    }


    private def rotateLeft(a: TreeNode[T]): TreeNode[T] = {
      val b = a.right.get
      a.right = b.left
      //set parent if node non-empty
      a.right.foreach(ar => ar.parent = Some(a)) // a.right.parent = a
      b.left = Some(a)
      a.parent = Some(b)
      return b
    }

    private def rotateRight(a: TreeNode[T]): TreeNode[T] = {
      val b = a.left.get
      a.left = b.right
      //set parent if node non-empty
      a.left.foreach(ar => ar.parent = Some(a))
      b.right = Some(a)
      a.parent = Some(b)
      return b
    }

    private def bigRotateLeft(a: TreeNode[T]): TreeNode[T] = {
      val b = a.right.get
      val c = b.left.get
      // val p = a.left
      val q = c.left
      val r = c.right
      //val s = b.right

      c.left = Some(a)
      a.parent = Some(c)

      c.right = Some(b)
      b.parent = Some(c)

      //a->p stay same
      a.right = q
      q.foreach(qn => qn.parent = Some(a))
      b.left = r
      r.foreach(rn => rn.parent = Some(b))
      //b->s stay same
      return c
      /*a.right = Some(rotateRight(a.right.get))
      a.right.get.parent = Some(a)

      return rotateLeft(a)
      */

    }

    private def bigRotateRight(a: TreeNode[T]): TreeNode[T] = {
      val b = a.left.get
      val c = b.right.get

      //val p = b.left
      val q = c.left
      val r = c.right
      //val s = a.right

      c.left = Some(b)
      b.parent = Some(c)

      c.right = Some(a)
      a.parent = Some(c)

      //b.left -> p stay
      b.right = q
      q.foreach(qn => qn.parent = Some(b))
      a.left = r
      r.foreach(qr => qr.parent = Some(a))
      return c
      //a.right -> s stay

      /*a.left = Some(rotateLeft(a.left.get))
      a.left.get.parent = Some(a)

      return rotateRight(a)*/
    }

    def traverseUpAndBalanceOnRemove(node: TreeNode[T], delta: Int): Unit = {
      node.balanceFactor += delta
      val balanced = balanceNode(node)
      balanced.parent match {
        case Some(parent) =>
          if (balanced.balanceFactor == 0) { //height changed go up
            traverseUpAndBalanceOnRemove(parent, if (cameFromLeft(parent, balanced)) -1 else 1)
          }
        case None =>
          balanced.parent = None
          root = Some(balanced)
      }
    }

    def traverseUpAndBalance(node: TreeNode[T], balanceDelta: Int): Unit = {
      node.balanceFactor += balanceDelta
      if (node.balanceFactor == 0) return
      val balanced = balanceNode(node)
      balanced.parent match {
        case Some(parent) =>
          if (balanced.balanceFactor == -1 || balanced.balanceFactor == 1) { //height changed go up
            traverseUpAndBalance(parent, if (cameFromLeft(parent, balanced)) 1 else -1)
          }
        case None =>
          balanced.parent = None
          root = Some(balanced)
      }
    }

    /** balance node if balance factor 2 or -2, changes links to parent */
    def balanceNode(node: TreeNode[T]): TreeNode[T] = {
      val current = node
      val parent = node.parent
      val leftChild: Boolean = parent match {
        case Some(p) => cameFromLeft(p, node)
        case None => false
      }

      //right subtree deeper (and exists)
      val res = if (current.balanceFactor == -2) {
        val pivot = node.right.get
        if (pivot.balanceFactor == -1) {
          current.balanceFactor = 0
          pivot.balanceFactor = 0
          rotateLeft(current)
        } else if (pivot.balanceFactor == 0) {
          current.balanceFactor = -1
          pivot.balanceFactor = 1
          rotateLeft(current)
        } else { //pivot.balanceFactor == 1 pivot.left subtree deeper (and exists)
          val bottom = pivot.left.get
          if (bottom.balanceFactor == 1) {
            current.balanceFactor = 0
            pivot.balanceFactor = -1
            bottom.balanceFactor = 0
          } else if (bottom.balanceFactor == -1) {
            current.balanceFactor = 1
            pivot.balanceFactor = 0
            bottom.balanceFactor = 0
          } else { //bottom.balanceFactor == 0
            current.balanceFactor = 0
            pivot.balanceFactor = 0
            bottom.balanceFactor = 0
          }
          bigRotateLeft(current)
        }
      } else if (current.balanceFactor == 2) { // left subtree deeper(and exists)
        val pivot = node.left.get
        if (pivot.balanceFactor == 1) { // pivot.left deeper
          current.balanceFactor = 0
          pivot.balanceFactor = 0
          rotateRight(current)
        } else if (pivot.balanceFactor == 0) {
          current.balanceFactor = 1
          pivot.balanceFactor = -1
          rotateRight(current)
        } else { //pivot.balanceFactor == -1 pivot.right subtree deeper (and exists)
          val bottom = pivot.right.get
          if (bottom.balanceFactor == -1) {
            current.balanceFactor = 0
            pivot.balanceFactor = 1
            bottom.balanceFactor = 0
          } else if (bottom.balanceFactor == 1) {
            current.balanceFactor = -1
            pivot.balanceFactor = 0
            bottom.balanceFactor = 0
          } else { //bottom.balanceFactor == 0
            current.balanceFactor = 0
            pivot.balanceFactor = 0
            bottom.balanceFactor = 0
          }
          bigRotateRight(current)
        }
      } else {
        current
      }
      res.parent = parent
      res.parent.foreach(p =>
        if (leftChild) {
          p.left = Some(res)
        } else {
          p.right = Some(res)
        })
      res
    }


    //Accessors e.t.c.

    @inline def notContains(value: T): Boolean = !contains(value)

    @inline def contains(value: T): Boolean = containsByPredicate(current => o.compare(value, current))

    def findNode(value: T, node: OptNode[T]): OptNode[T] = node match {
      case Some(n) =>
        val cmp = o.compare(value, n.value) //-1 go left
        if (cmp < 0) findNode(value, n.left)
        else if (cmp > 0) findNode(value, n.right)
        else node
      case None => None
    }

    def findNode(value: T): OptNode[T] = findNode(value, root)

    /** 0 - empty tree */
    def height: Int = height(root)

    def height(node: OptNode[T]): Int = node match {
      case Some(n) => 1 + math.max(height(n.left), height(n.right))
      case None => 0
    }


    def traverseWithDeepness(f: (T, Int) => Unit): Unit = traverseWithDeepness(root, 1, f)

    def traverseWithDeepness(node: OptNode[T], deepness: Int, f: (T, Int) => Unit): Unit = node.foreach {
      n =>
        f(n.value, deepness)
        traverseWithDeepness(n.left, deepness + 1, f)
        traverseWithDeepness(n.right, deepness + 1, f)
    }


    /**
      * @param pred takes node value, returns -1 go left 0 return true 1 go right
      */
    def findByPredicate(pred: T => Int): OptNode[T] = {
      var current = root
      while (current.isDefined) {
        val cmp = pred(current.get.value) //o.compare( value, current.get.value)
        if (cmp == 0) return current
        if (cmp == -1) {
          current = current.get.left
        } else {
          current = current.get.right
        }
      }
      return None
    }

    /**
      * @param pred takes node value, returns -1 go left 0 return true 1 go right
      */
    def containsByPredicate(pred: T => Int): Boolean = findByPredicate(pred).isDefined

    /**
      * @param pred takes node value, returns -1 go left 0 return true 1 go right
      *             go deeper while can returns last visited before null
      */
    def closestNodeByPredicate(pred: T => Int): OptNode[T] = {
      if (root.isEmpty) return None
      var current = root.get
      while (true) {
        val cmp = pred(current.value) //o.compare( value, current.get.value)
        if (cmp == 0) return Some(current)
        if (cmp == -1 && current.left.isDefined) {
          current = current.left.get
        } else if (cmp == 1 && current.right.isDefined) {
          current = current.right.get
        } else {
          return Some(current)
        }
      }
      return None
    }

    def minNode: OptNode[T] = root match {
      case Some(node) =>
        var current = node
        while (current.left.isDefined) {
          current = current.left.get
        }
        Some(current)
      case None => None
    }

    def maxNode: OptNode[T] = root match {
      case Some(node) =>
        var current = node
        while (current.left.isDefined) {
          current = current.left.get
        }
        Some(current)
      case None => None
    }

    def traverseNodes(f: TreeNode[T] => Unit): Unit = {
      val toVisit: mutable.Queue[OptNode[T]] = new mutable.Queue[OptNode[T]]()
      toVisit += root
      while (toVisit.nonEmpty) {
        val cur = toVisit.dequeue()
        cur.foreach {
          node =>
            f(node)
            toVisit += node.left
            toVisit += node.right
        }
      }
    }

    def values: Iterator[T] = valuesOrdered

    def valuesOrdered: Iterator[T] = minNode.greaterOrEquals

    def valuesOrderedReversed: Iterator[T] = maxNode.lesserOrEquals

    def min: Option[T] = minNode.map(_.value)

    def max: Option[T] = maxNode.map(_.value)


  }

  implicit class NodeIterators[T](val node: OptNode[T]) extends AnyVal {
    def greaterOrEquals: Iterator[T] = forwardIterator(node)

    def lesserOrEquals: Iterator[T] = backwardIterator(node)

    def greater: Iterator[T] = node match {
      case Some(n) => forwardIterator(n.next)
      case None => Iterator.empty
    }

    def lesser: Iterator[T] = node match {
      case Some(n) => backwardIterator(n.prev)
      case None => Iterator.empty
    }
  }

  /** Iterator to traverse nodes from given node using .next */
  def forwardIterator[T](node: OptNode[T]): Iterator[T] = new Iterator[T] {
    var current: OptNode[T] = node

    override def hasNext: Boolean = current.isDefined

    override def next(): T = {
      val res = current.get
      current = res.next
      return res.value
    }
  }

  /** Iterator to traverse nodes from given node  using .prev */
  def backwardIterator[T](n: OptNode[T]): Iterator[T] = new Iterator[T] {
    var current: OptNode[T] = n

    override def hasNext: Boolean = current.isDefined

    override def next(): T = {
      val res = current.get
      current = res.next
      return res.value
    }
  }

}

