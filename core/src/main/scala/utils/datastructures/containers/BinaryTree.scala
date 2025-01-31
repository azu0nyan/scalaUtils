package utils.datastructures.containers

import scala.annotation.tailrec

object BinaryTree {
  sealed trait BinaryTree[+A] {

    def elements:Seq[A] = this match {
      case Node(a, left, right) => left.elements ++ Seq(a) ++ right.elements
      case Leaf(a) => Seq(a)
      case EmptyTree => Seq()
    }

    override def toString: String = elements.mkString(s"Tree(", ",", ")")

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
        case Node(_, _, right) => right.contains(toFind)
        case Leaf(a) => a == toFind
        case EmptyTree => false
      }

    /**Assumes that condition depends on elements order*/
    def maximumSatisfiesCondition(cond: A => Boolean): Option[A] =
      this match {
        case Node(a, _, right) if cond(a) => right.maximumSatisfiesCondition(cond).orElse(Some(a))
        case Node(_, left, _) => left.maximumSatisfiesCondition(cond)
        case Leaf(a) if cond(a) => Some(a)
        case Leaf(_) => None
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

}
