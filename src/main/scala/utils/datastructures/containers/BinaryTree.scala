package utils.datastructures.containers

import scala.annotation.tailrec

object BinaryTree {
  sealed trait BinaryTree[+A] {
    def add[A1 >: A](toAdd: A1)(implicit ord: Ordering[A1]): BinaryTree[A1] =
      this match {
        case Node(a, left, right) if ord.lt(toAdd, a) => Node(a, left.add(toAdd), right)
        case Node(a, left, right) => Node(a, left, right.add(toAdd))
        case Leaf(a) if ord.lt(toAdd, a) => Node(a, Leaf(toAdd), EmptyTree)
        case Leaf(a) => Node(a, EmptyTree, Leaf(toAdd))
        case EmptyTree => Leaf(toAdd)
      }

    @tailrec
    final def minOpt: Option[A] =
      this match {
        case Node(a, EmptyTree, _) => Some(a)
        case Node(_, left, _) => left.minOpt
        case Leaf(a) => Some(a)
        case EmptyTree => None
      }

    @tailrec
    final def min: A =
      this match {
        case Node(a, EmptyTree, _) => a
        case Node(_, left, _) => left.min
        case Leaf(a) => a
        case EmptyTree => throw new NoSuchElementException()
      }

    @tailrec
    final def max: A =
      this match {
        case Node(a, _, EmptyTree) => a
        case Node(_, _, right) => right.max
        case Leaf(a) => a
        case EmptyTree => throw new NoSuchElementException()
      }


    @tailrec
    final def maxOpt: Option[A] =
      this match {
        case Node(a, _, EmptyTree) => Some(a)
        case Node(_, _, right) => right.maxOpt
        case Leaf(a) => Some(a)
        case EmptyTree => None
      }

    @tailrec
    final def contains[A1 >: A](toFind: A1)(implicit ord: Ordering[A1]): Boolean =
      this match {
        case Node(a, _, _) if a == toFind => true
        case Node(a, left, _) if ord.lt(toFind, a) => left.contains(toFind)
        case Node(a, _, right) => right.contains(toFind)
        case Leaf(a) => a == toFind
        case EmptyTree => false
      }

    def closestLessB[B](b: B, less: (A, B) => Boolean): Option[A] =
      this match {
        case Node(a, left, right) if less(a, b) => right.closestLessB(b, less).orElse(Some(a))
        case Node(a, left, right) => left.closestLessB(b, less)
        case Leaf(a) if less(a, b) => Some(a)
        case Leaf(a) => None
        case EmptyTree => None
      }

    def +=[A1 >: A](toAdd: A1)(implicit ord: Ordering[A1]): BinaryTree[A1] = add(toAdd)
    def -=[A1 >: A](toRemove: A1)(implicit ord: Ordering[A1]): BinaryTree[A1] = remove(toRemove)
    def remove[A1 >: A](toRemove: A1)(implicit ord: Ordering[A1]): BinaryTree[A1] =
      this match {
        case Node(a, EmptyTree, right) if a == toRemove => right
        case Node(a, left, EmptyTree) if a == toRemove => left
        case Node(a, left, right) if a == toRemove =>
          val maxLeft = left.max
          Node(maxLeft, left.remove[A1](maxLeft), right)
        case Node(a, left, right) if ord.lt(toRemove, a) => Node(a, left.remove(toRemove), right)
        case Node(a, left, right) => Node(a, left, right.remove(toRemove))
        case Leaf(a) if a == toRemove => EmptyTree
        case Leaf(a) => Leaf(a)
        case EmptyTree => EmptyTree
      }

    def size: Int = this match {
      case Node(_, left, right) => 1 + left.size + right.size
      case Leaf(_) => 1
      case EmptyTree => 0
    }

  }
  case class Node[A](a: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]
  case class Leaf[A](a: A) extends BinaryTree[A]
  case object EmptyTree extends BinaryTree[Nothing]

  //  def add[A](tree: BinaryTree[A], toAdd: A)(implicit ord: Ordering[A]): BinaryTree[A] =
  //    tree match {
  //      case Node(a, left, right) if ord.lt(toAdd, a) => Node(a, add(left, a), right)
  //      case Node(a, left, right) => Node(a, left, add(right, a))
  //      case Leaf(a) if ord.lt(toAdd, a) => Node(a, Leaf(toAdd), Empty)
  //      case Leaf(a) => Node(a, Empty, Leaf(toAdd))
  //      case Empty => Leaf(toAdd)
  //    }
  //
  //  @tailrec
  //  def minOpt[A](tree: BinaryTree[A]): Option[A] =
  //    tree match {
  //      case Node(a, Empty, _) => Some(a)
  //      case Node(_, left, _) => minOpt(left)
  //      case Leaf(a) => Some(a)
  //      case Empty => None
  //    }

  //  @tailrec
  //  def min[A](tree: BinaryTree[A]): A =
  //    tree match {
  //      case Node(a, Empty, _) => a
  //      case Node(_, left, _) => min(left)
  //      case Leaf(a) => a
  //      case Empty => throw new NoSuchElementException()
  //    }
  //
  //  @tailrec
  //  def max[A](tree: BinaryTree[A]): A =
  //    tree match {
  //      case Node(a, _, Empty) => a
  //      case Node(_, _, right) => max(right)
  //      case Leaf(a) => a
  //      case Empty => throw new NoSuchElementException()
  //    }
  //
  //
  //  @tailrec
  //  def maxOpt[A](tree: BinaryTree[A]): Option[A] =
  //    tree match {
  //      case Node(a, _, Empty) => Some(a)
  //      case Node(_, _, right) => maxOpt(right)
  //      case Leaf(a) => Some(a)
  //      case Empty => None
  //    }


  //  @tailrec
  //  def find[A](tree: BinaryTree[A], toFind: A)(implicit ord: Ordering[A]): Boolean =
  //    tree match {
  //      case Node(a, left, right) if a == toFind => true
  //      case Node(a, left, right) if ord.lt(toFind, a) => find(left, toFind)
  //      case Node(a, left, right) => find(right, toFind)
  //      case Leaf(a) => a == toFind
  //      case Empty => false
  //    }


  //  def remove[A](tree: BinaryTree[A], toRemove: A)(implicit ord: Ordering[A]): BinaryTree[A] =
  //    tree match {
  //      case Node(a, Empty, right) if a == toRemove => right
  //      case Node(a, left, Empty) if a == toRemove => left
  //      case Node(a, left, right) if a == toRemove =>
  //        val maxLeft = max(left)
  //        Node(maxLeft, remove(left, maxLeft), right)
  //      case Node(a, left, right) if ord.lt(toRemove, a) => Node(a, remove(left, toRemove), right)
  //      case Node(a, left, right) => Node(a, left, remove(right, toRemove))
  //      case Leaf(a) if a == toRemove => Empty
  //      case Leaf(a) => Leaf(a)
  //      case Empty => Empty
  //    }

  //  def closestLess[A, B](tree: BinaryTree[A], b: B, less: (A, B) => Boolean): Option[A] =
  //    tree match {
  //      case Node(a, left, right) if less(a, b) => closestLess(right, b, less).orElse(Some(a))
  //      case Node(a, left, right) => closestLess(left, b, less)
  //      case Leaf(a) if less(a, b) => Some(a)
  //      case Leaf(a) => None
  //      case Empty => None
  //    }
}
