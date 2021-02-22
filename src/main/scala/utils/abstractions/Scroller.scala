package utils.abstractions

import utils.system.Event.{Event, EventImpl}

class Scroller[T](
               var current:T,
              val variantOrder:Map[T, T],
              ) {

  val reverseOrder:Map[T, T] = variantOrder.toSeq.map(_.swap).toMap

  def next():T = {
    current = variantOrder(current)
    onValueChanged(current)
    current
  }

  def prev():T = {
    current = reverseOrder(current)
    onValueChanged(current)
    current
  }

  val onValueChanged:Event[T] = new EventImpl[T] {}

}
