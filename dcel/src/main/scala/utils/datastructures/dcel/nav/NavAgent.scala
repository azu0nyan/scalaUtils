package utils.datastructures.dcel.nav

import utils.math.Scalar
import utils.math.planar.V2
import utils.system.CalcExecTime.CalcTimeStats
import utils.datastructures.dcel.nav.NavigableDCEL._
import utils.datastructures.dcel.nav.DCELPath._


object NavAgent {

//  import org.slf4j.LoggerFactory
//
//  val log = LoggerFactory.getLogger(this.getClass)

//  def makeAt(position: V2, rotation: Scalar, area: NavigableFace): NavAgent = {
//    val realArea = area.areaAt(position)
//    val res = new NavAgent(position, rotation, realArea)
//    realArea.navAgentsData.enqueueAddAgent(res)
//    res
//  }

  val findPathTime = new CalcTimeStats(Some("Find path time "), Some(3000))
}

class NavAgent private(
                        var position: V2,
                        var rotation: Scalar,
                        var inArea: NavigableFace,
                      ) {


  var maxSpeed: Scalar = 50

  var currentPath: Option[DCELPath] = None

  var pathNotFound: Boolean = false

  var expandedToNextNode: Option[Seq[V2]] = None

  var destination: Option[V2] = None

  /** todo maybe data:Option[UserData] from NavAgent[UserData] */
//  var creature: Option[Creature] = None

  def destinationReached: Boolean = currentPath.isEmpty

//  def teleport(point: V2): Unit = {
    //log.debug(s"Teleporting to $point")
//    val toArea = inArea.areaAt(point)
//    if (toArea != inArea)
//      transferToArea(point, toArea)
//    else position = point
//  }
/*
  def transferToArea(point: V2, other: NavigableFace): Unit = {
    //log.debug(s"Transferring to area $other")
    inArea.navAgentsData.removeAgentInHisUpdateFrame(this)
    other.navAgentsData.enqueueAddAgent(this)
    position = point
    inArea = other
  }


  def goTo(point: V2): Unit = /*this.synchronized*/ {
    //log.debug(s"New target location set $point")
    pathNotFound = false
    if (!(point ~= position)) {
      destination = Some(point)
      findPathTime {
        currentPath = inArea.findPath(position, point)
        if(currentPath.isEmpty) pathNotFound = true
      }
      //log.trace(s"Path found $currentPath")
    }
  }*/
  //todo concurrent
  def setNoDestination(): Unit = {
    destination = None
    currentPath = None
  }

  var paused: Boolean = false

  def pauseMovement(): Unit = paused = true

  def continueMovement(): Unit = paused = false

  private def moveInDirectionUnsafe(from: V2, to: V2, dist: Scalar): V2 = from + (to - from).normalize * dist

/*
  def update(tpf: Scalar): Unit = /*this.synchronized*/
   if (!paused) {
      var distanceToWalk = maxSpeed * tpf
      var newPath = currentPath
      var newPosition = position
      var newArea = inArea
      while (distanceToWalk > 0 && newPath.nonEmpty) {
        for (path <- newPath;
             edge <- path.edges.headOption) {
          //        //log.trace(s"Going trough $edge $path")
          edge match {
            case GoBetweenPoints(from, to) =>
              if (edge.length <= distanceToWalk) {
                distanceToWalk -= edge.length
                newPosition = to.point
                newPath = path.dropFirstNode
              } else {
                newPosition = moveInDirectionUnsafe(from.point, to.point, distanceToWalk)
                distanceToWalk = 0
                newPath = Some(path.replaceHead(Seq(GoBetweenPoints(PointNode(newPosition, from.area), to))))
              }
            case GoToPoint(from, to) =>
              if (edge.length <= distanceToWalk) {
                distanceToWalk -= edge.length
                newPosition = to.point
                newPath = path.dropFirstNode
              } else {
                distanceToWalk = 0
                newPosition = moveInDirectionUnsafe(from.point, to.point, distanceToWalk)
                newPath = Some(path.replaceHead(Seq(GoBetweenPoints(PointNode(newPosition, from.area), to))))
              }
            case gfp@GoFromPoint(from, to) =>
              if (edge.length <= distanceToWalk) {
                distanceToWalk -= edge.length
                newPosition = to.point
                newPath = path.dropFirstNode
              } else {
                distanceToWalk = 0
                newPosition = moveInDirectionUnsafe(from.point, to.point, distanceToWalk)
                newPath = Some(path.replaceHead(Seq(gfp.copy(from = gfp.from.copy(point = newPosition)))))
              }
            case GoAlongSameBorder(from, to) => //todo check this source of errors, check inArea correctness
              if (edge.length <= distanceToWalk) {
                distanceToWalk -= edge.length
                newPosition = to.point
                newPath = path.dropFirstNode
              } else {
                distanceToWalk = 0
                newPosition = moveInDirectionUnsafe(from.point, to.point, distanceToWalk)
                newPath = Some(path.replaceHead(Seq(GoFromPoint(PointNode(newPosition, from.area), to))))
              }
            case GoTroughPortal(from, to) if inArea != to.area =>
              newPath = path.dropFirstNode
              newArea = from.travelsToArea
              newPosition = from.toPoint
            case GoTroughPortal(from, to) =>
              newPath = path.dropFirstNode
              newPosition = from.toPoint
            case GoTroughBorder(from, to) if inArea != to.area =>
              newPath = path.dropFirstNode
              newArea = to.area
            case GoTroughBorder(from, to) =>
              newPath = path.dropFirstNode
            case GoToParentBorder(from, to) if inArea != to.area =>
              newArea = to.area
              newPath = path.dropFirstNode
            case GoToParentBorder(from, to) =>
              newPath = path.dropFirstNode
            case GoToChildBorder(from, to) if inArea != to.area =>
              newArea = to.area
              newPath = path.dropFirstNode
            case GoToChildBorder(from, to) =>
              newPath = path.dropFirstNode
            case GoTroughAreaOnVisGraph(from, to, a, length, Some(visGraphPath)) =>
              newPath = Some(path.replaceHead(visGraphPath.edges))
            case GoTroughAreaOnVisGraph(from, to, a, length, None) =>
              a.navData.findPathOnVisibilityGraph(from.point, to.point) match {
                case Some(replacement) => newPath = Some(path.replaceHead(replacement.edges))
                case None => //todo path invalidation
                  //log.error(s"Cant find path on visibility graph at ${a.data.name} from ${from.point} to ${to.point}.")
              }
            case GoTroughArea(from, to, a, length, Some(areaPath)) /*if from.area == to.area*/ =>
              newPath = Some(path.replaceHead(areaPath.edges))
            case GoTroughArea(from, to, a, length, None) /*if from.area == to.area*/ =>
              //a.findPathOnEdgeGraph(from, to) //todo if found path contains edge
              a.navData.findPathInsideLowest(from.point, to.point, length) match {
                case Some(replacement) =>
                  //log.trace(s"Found replacement $replacement")
                  newPath = Some(path.replaceHead(replacement.edges))
                case None => //todo path invalidation
                  //log.info(s"Cant found path")
              }
          }
        }
      }
      currentPath = newPath
      if (newArea != inArea) transferToArea(newPosition, newArea)
      else if (position != newPosition) position = newPosition

    }*/

}
