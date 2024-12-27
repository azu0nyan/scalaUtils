package utils.math.planar.algo.straightSkeleton


import java.awt.Color
import java.util
import java.util.{Collections, List, Set}


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
class Machine(initial: Double) {
  color = Machine.rainbow(Machine.rainbowIndex % Machine.rainbowStrings.length)
  description = Machine.rainbowStrings(Machine.rainbowIndex % Machine.rainbowStrings.length)
  Machine.rainbowIndex += 1
  addHeightEvent(new DirectionHeightEvent(this, initial))
  currentAngle = initial
  var color: Color = null // color used in the ui

  // a machine will only ever have one pending event in the skeleton.qu, others are stored here
  var events = new ArrayList[_]
   var description = "unnamed machine"
  var currentAngle = Math.PI / 4 // when a edge is added this is the angle it is given

  @transient var heightEvent: HeightEvent = null
  @transient var currentDirection = -1
  protected var seenEdges = new LinkedHashSet[_]
  def this {
    this(Math.PI / 4)
  }
  override def toString = description
  /**
   * Called once after the machine is assigned to it's first edge
   */
  def addEdge(e: Edge, skel: Skeleton): Unit = {
    if (heightEvent == null) findNextHeight(skel)
    // when we're new, or sometimes when we swap a machine out and back in again things get confused
    //        if ( seenEdges.add( e ) || e.getAngle() != currentAngle )
    //        {
    e.setAngle(currentAngle)
    //        }
  }
  def findOurEdges(skel: Skeleton) = {
    val edgesToChange = new ArrayList[_]

    for (e <- skel.liveEdges) {
      if (e.machine eq this) edgesToChange.add(e)
    }
    edgesToChange
  }
  def findNextHeight(skel: Skeleton): Unit = {
    if (events.isEmpty) throw new Error("I need height events!")
    currentDirection += 1
    if (currentDirection == 0) {
      if (events.get(0).isInstanceOf[DirectionHeightEvent]) {
        // first direction added to a new edge is taken to be the starting angle
        currentAngle = events.get(0).asInstanceOf[DirectionHeightEvent].newAngle
        heightEvent = events.get(0)
        currentDirection += 1
      }
      else throw new Error("You have to think really hard about how the first event sets it's angle before you do this") // I ran into trouble - as we add edges, we want to be able to set the initial angle and ignore the first height event.
      // Otherwise we immediately call replace edges in skeleton and introduce additional edges

    }
    if (currentDirection >= getDirections.size) return
    heightEvent = getDirections.get(currentDirection)
    skel.qu.add(heightEvent)
  }
  /**
   * @return the directions
   */
  def getDirections = events
  def sortHeightEvents(): Unit = {
    Collections.sort(events, HeightEvent.heightComparator)
  }
  /**
   * @param directions the directions to set
   */
  def addHeightEvent(dir: HeightEvent): Unit = {
    events.add(dir)
    sortHeightEvents()
  }
}

