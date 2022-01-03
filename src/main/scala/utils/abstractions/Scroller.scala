package utils.abstractions

import utils.system.Event.{Event, EventImpl}

class Scroller[T](
                   var current: T,
                   val variantOrder: Map[T, T],
                 ) {
  def this(current: T, orderSeq: Seq[T]) = {
    this(current, (orderSeq :+ orderSeq.head).sliding(2).map { case Seq(a, b) => (a, b) } toMap)
  }

  val reverseOrder: Map[T, T] = variantOrder.toSeq.map(_.swap).toMap

  def next(): T = {
    val old = current
    current = variantOrder(current)
    onValueChanged((old, current))
    current
  }

  def prev(): T = {
    val old = current
    current = reverseOrder(current)
    onValueChanged((old, current))
    current
  }
  
  /**old then new*/
  val onValueChanged: Event[(T, T)] = new EventImpl[(T, T)] {}

}
