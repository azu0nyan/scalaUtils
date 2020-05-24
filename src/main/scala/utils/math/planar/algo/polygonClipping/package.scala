package utils.math.planar.algo

import java.util.logging.Logger

import utils.datastructures.containers.DoubleLinkedList
import utils.math._
import utils.math.planar.V2

/**
  * based on
  * http://sean.cm/a/polygon-clipping-pt2
  * https://github.com/voidqk/polybooljs
  */
package object polygonClipping {

  val log:Logger = Logger.getLogger("PolygonClipping")

  type Region = Seq[V2]

  case class Poly( regions: Iterable[Region], inverted: Boolean = false)

  class Epsilon(val e: Scalar) extends AnyVal

  implicit def epsilonSugar(e: Epsilon): Scalar = e.e

  class Fill(var above:Option[Boolean], var below:Option[Boolean]) {
    def notAbove:Option[Boolean] = Some(above  match {/*!eve.seg.myFill.above;*/
      case Some(b) => !b
      case None =>true
    })

    def notBelow:Option[Boolean] = Some(below  match {/*!eve.seg.myFill.below;*/
      case Some(b) => !b
      case None =>true
    })

    override def toString = s"Fill(above=$above, below=$below)"
  }

  private[polygonClipping] class Segment(
                                          var start: V2,
                                          var end: V2,
                                          var myFill:Fill = new Fill(None, None),
                                          var otherFill:Fill = null
                                        ) {
    def segmentCopy(newStart: V2, newEnd: V2): Segment =
      new Segment(newStart, newEnd, new Fill(myFill.above, myFill.below), null) /*    otherFill: null*/


    override def toString = s"Segment($start, $end, $myFill, $otherFill)"
  }

  /**
    * @param isStart a flag that distinguishes between Start and End events
    * @param pt      the point associated with this event
    * @param seg     the segment that generated this event
    * @param other   the sister event (Start’s other points to its End, and vice-versa)
    * @param primary ???????????????????
    *                //@param status  status – the future node inside the status stack
    *
    */
  private[polygonClipping] class Event(
                                        var isStart: Boolean,
                                        var pt: V2,
                                        var seg: Segment,
                                        var other: Event,
                                        var primary: Boolean,
                                        var status:DoubleLinkedList.Node[Event] = null,
                                        //var status:DoubleLinkedList.Node[Event] = null,
                                        var myNode: DoubleLinkedList.Node[Event] = null
                                      )

}
