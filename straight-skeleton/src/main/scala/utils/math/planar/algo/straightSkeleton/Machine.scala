package utils.math.planar.algo.straightSkeleton


import java.awt.Color
import java.util
import java.util.{Collections, Comparator, List, Set}
import scala.collection.mutable
import scala.jdk.CollectionConverters.BufferHasAsJava


/**
 * A machine controls the angle of it's set of edges over time (height...)
 *
 * superclass of all machines
 *
 * idea is to add all directions before adding edges. When you add the first edge
 *
 * instead of just adding all height changes to the main event queue, this
 * machine only adds the next one. Reason here is that we might want to change
 * our minds as we build upwards.
 *
 * @author twak
 */
object Machine { // for pretty output
  val rainbow = Array[Color](Color.red, Color.green, Color.blue, Color.magenta)
  val rainbowStrings = Array[String]("red", "green", "blue", "magenta")
  var rainbowIndex = 0
}

class Machine(initial: Double = Math.PI / 4) {
  var heightEvent: Option[HeightEvent] = None
  var currentDirection = -1
  var seenEdges = new mutable.LinkedHashSet[Edge]

  var color = Machine.rainbow(Machine.rainbowIndex % Machine.rainbowStrings.length)
  var description = Machine.rainbowStrings(Machine.rainbowIndex % Machine.rainbowStrings.length)
  Machine.rainbowIndex += 1
  addHeightEvent(new DirectionHeightEvent(this, initial))
  var currentAngle = initial // // when a edge is added this is the angle it is given

  // a machine will only ever have one pending event in the skeleton.qu, others are stored here
  var events = mutable.Buffer[HeightEvent]()


  override def toString = description
  /**
   * Called once after the machine is assigned to it's first edge
   */
  def addEdge(e: Edge, skel: Skeleton): Unit = {
    if (heightEvent.isEmpty) findNextHeight(skel)
    // when we're new, or sometimes when we swap a machine out and back in again things get confused
    //        if ( seenEdges.add( e ) || e.getAngle() != currentAngle )
    //        {
    e.setAngle(currentAngle)
    //        }
  }

  def findOurEdges(skel: Skeleton) = {
    val edgesToChange = mutable.Buffer[Edge]()

    for (e <- skel.liveEdges) {
      if (e.machine eq this)
        edgesToChange += e
    }
    edgesToChange
  }

  def findNextHeight(skel: Skeleton): Unit = {
    if (events.isEmpty)
      throw new Error("I need height events!")
    currentDirection += 1
    if (currentDirection == 0) {
      events.head match
        case event: DirectionHeightEvent =>
          // first direction added to a new edge is taken to be the starting angle
          currentAngle = event.newAngle
          heightEvent = Some(events.head)
          currentDirection += 1
        case _ => throw new Error("You have to think really hard about how the first event sets it's angle before you do this") // I ran into trouble - as we add edges, we want to be able to set the initial angle and ignore the first height event.
      // Otherwise we immediately call replace edges in skeleton and introduce additional edges

    }
    if (currentDirection >= getDirections.size) {
      //do nothing
    } else {
      heightEvent = Some(getDirections(currentDirection))
      skel.qu.add(heightEvent.get)
    }
  }
  /**
   * @return the directions
   */
  def getDirections = events
  def sortHeightEvents(): Unit =
    events.asJava.sort(Comparator.comparingDouble(_.getHeight))

  /**
   * @param directions the directions to set
   */
  def addHeightEvent(dir: HeightEvent): Unit = {
    events += dir
    sortHeightEvents()
  }
}

