package utils.datastructures.containers

import scala.annotation.tailrec

object BinaryTree {
  sealed trait BinaryTree[+A]
  case class Node[A](a: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]
  case class Leaf[A](a: A) extends BinaryTree[A]
  case object Empty extends BinaryTree[Nothing]

  def add[A](tree: BinaryTree[A], toAdd: A)(implicit ord: Ordering[A]): BinaryTree[A] =
    tree match {
      case Node(a, left, right) if ord.lt(toAdd, a) => Node(a, add(left, a), right)
      case Node(a, left, right) if ord.lt(toAdd, a) => Node(a, left, add(right, a))
      case Leaf(a) if ord.lt(toAdd, a) => Node(a, Leaf(toAdd), Empty)
      case Leaf(a) => Node(a, Empty, Leaf(toAdd))
      case Empty => Leaf(toAdd)
    }

  @tailrec
  def minOpt[A](tree: BinaryTree[A]): Option[A] =
    tree match {
      case Node(a, Empty, _) => Some(a)
      case Node(_, left, _) => minOpt(left)
      case Leaf(a) => Some(a)
      case Empty => None
    }

  @tailrec
  def min[A](tree: BinaryTree[A]): A =
    tree match {
      case Node(a, Empty, _) => a
      case Node(_, left, _) => min(left)
      case Leaf(a) => a
      case Empty => throw new NoSuchElementException()
    }

  @tailrec
  def max[A](tree: BinaryTree[A]): A =
    tree match {
      case Node(a, _, Empty) => a
      case Node(_, _, right) => max(right)
      case Leaf(a) => a
      case Empty => throw new NoSuchElementException()
    }


  @tailrec
  def maxOpt[A](tree: BinaryTree[A]): Option[A] =
    tree match {
      case Node(a, _, Empty) => Some(a)
      case Node(_, _, right) => maxOpt(right)
      case Leaf(a) => Some(a)
      case Empty => None
    }


  @tailrec
  def find[A](tree: BinaryTree[A], toFind: A)(implicit ord: Ordering[A]): Boolean =
    tree match {
      case Node(a, left, right) if a == toFind => true
      case Node(a, left, right) if ord.lt(toFind, a) => find(left, toFind)
      case Node(a, left, right) => find(right, toFind)
      case Leaf(a) => a == toFind
      case Empty => false
    }


  def remove[A](tree: BinaryTree[A], toRemove: A)(implicit ord: Ordering[A]): BinaryTree[A] =
    tree match {
      case Node(a, Empty, right) if a == toRemove => right
      case Node(a, left, Empty) if a == toRemove => left
      case Node(a, left, right) if a == toRemove =>
        val maxLeft = max(left)
        Node(maxLeft, remove(left, maxLeft), right)
      case Node(a, left, right) if ord.lt(toRemove, a) => Node(a, remove(left, toRemove), right)
      case Node(a, left, right) => Node(a, left, remove(right, toRemove))
      case Leaf(a) if a == toRemove => Empty
      case Leaf(a) => Leaf(a)
      case Empty => Empty
    }

  def closestLess[A, B](tree: BinaryTree[A], b: B, less: (A, B) => Boolean): Option[A] =
    tree match {
      case Node(a, left, right) if less(a, b) => closestLess(right, b, less).orElse(Some(a))
      case Node(a, left, right) => closestLess(left, b, less)
      case Leaf(a) if less(a, b) => Some(a)
      case Leaf(a) => None
      case Empty => None
    }
}
