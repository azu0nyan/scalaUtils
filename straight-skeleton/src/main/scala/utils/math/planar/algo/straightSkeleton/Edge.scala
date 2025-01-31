package utils.math.planar.algo.straightSkeleton

import utils.math.planar.V2
import utils.math.planar.algo.straightSkeleton.implhelpers.{Cache, ConsecutivePairs, Loop, LoopL}
import utils.math.planar.algo.straightSkeleton.math.{LinearForm3D, Ray3d}
import utils.math.space.V3
import utils.math.space.V3
import utils.math.space.V3.toNormal

import scala.collection.mutable

/**
 * note: the defn of weight here is a little strange, can
 * alter it later as long as the uphill vector calculation
 * is updated too.
 *
 * @author twak
 */
object Edge {
  /**
   * reverses the start & ends of each edge in a loop, but doesn't change the
   * corner-chain order (this is done by skeleton as it sets up)
   *
   */
  def reverse(loopl: LoopL[Edge]): Unit = {

    for (e <- loopl.eIterator) {
      var tmp = e.start
      e.start = e.end
      e.end = tmp
      tmp = e.start.nextC
      e.start.nextC = e.start.prevC
      e.start.prevC = tmp
      val et = e.start.nextL
      e.start.nextL = e.start.prevL
      e.start.prevL = et
    }

    for (l <- loopl.iterator) {
      l.reverse
    }
  }
  def fromPoints(ribbon: List[V2]) = {
    val loop = new Loop[Edge]
    val cache = new Cache[V2, Corner]() {
      override def create(i: V2) = new Corner(i.x, i.y)
    }

    for (pair <- new ConsecutivePairs[V2](ribbon, true)) {
      loop.append(new Edge(cache.get(pair._1), cache.get(pair._2)))
    }
    loop
  }
  /**
   * UNTESTED!
   *
   * @param ribbon
   * @return
   */
  def fromPoints2d(ribbon: LoopL[V2]) = {
    val loopl = new LoopL[Edge]
    val cache = new Cache[V2, Corner]() {
      override def create(i: V2) = new Corner(i.x, i.y)
    }

    for (pLoop <- ribbon.iterator) {
      val loop = new Loop[Edge]
      loopl.add(loop)

      for (pair <- pLoop.loopableIterator) {
        loop.append(new Edge(cache.get(pair.get), cache.get(pair.getNext.get)))
      }
    }
    loopl
  }


  def uniqueEdges(corners: LoopL[Corner]) = {
    val cs = new mutable.HashSet[Edge]

    for (c <- corners.eIterator) {
      cs.add(c.nextL)
    }
    cs
  }
  /**
   *
   * This is a robust collision of a's adjacent edges with a horizontal plane at the given height
   * When Two parallel edges are given, we can assume that the
   */
  def collide(a: Corner, height: Double): V3 = {
    val ceiling = new LinearForm3D(0, 0, 1, -height)
    // this can cause Jama not to return...
    if (a.prevL.linearForm.hasNaN || a.nextL.linearForm.hasNaN) throw new Error
    try ceiling.collide(a.prevL.linearForm, a.nextL.linearForm).get
    catch {
      case e: RuntimeException =>
        assert(a.prevL.sameDirectedLine(a.nextL))
        // a vector in the direction of uphill from a
        // via similar triangle (pyramids)
        //assume, they're all coincident?
        val dir = JavaCompat.normalizeJava(a.prevL.uphill) * (height - a.z) + a.asV3
        new V3(dir.x, dir.y, height)
    }
  }
  /**
   * Collides a's two adjacent edges against the other given edge.
   *
   * Currently this handles the case that a.next, a.prev is
   *
   * @return
   */
  def collide(a: Corner, edge: Edge): V3 = throw new UnsupportedOperationException("Not yet implemented")
}

/**
 * adds an output side from start to end => Edges are immutable!
 */
class Edge(var start: Corner, var end: Corner) {
  //        addOutputSide ( start, end );
  // 0 is straight up, positive/-ve is inwards/outwards, absolute value must be less than Math.PI/2
  private var angle = Math.PI / 4
  // orthogonal vector pointing uphill
  var uphill: V3 = null
  var linearForm: LinearForm3D = null
  // corners that currently reference this edge in prevL or nextL
  var currentCorners = new mutable.LinkedHashSet[Corner]
  var machine: Machine = null
  // features that this edge has been tagged with
  var profileFeatures = new mutable.LinkedHashSet[Tag]

  def this(start: Corner, end: Corner, angle: Double) = {
    this(start, end)
    this.angle = angle
    calculateLinearForm()
  }
  def this(start: V3, end: V3, angle: Double) = {
    this(new Corner(start), new Corner(end), angle)
  }

  /**
   * The perpendicular unit vector pointing up the slope of the side
   */
  private def calculateUphill(): Unit = {
    // perpendicular in x,y plane
    // horizontal component
    // vertical component
    uphill = JavaCompat.normalizeJava(new V3(-direction.y, direction.x, 0)) * Math.sin(getAngle) + new V3(0, 0, Math.cos(getAngle))
  }
  /**
   * finds the Ax + By + Cz = D form of the edge
   * Called when the the weight of the edge changes
   */
  def calculateLinearForm(): Unit = {
    calculateUphill()
    // find normal from uphill and edge
    val norm = getPlaneNormal
    linearForm = new LinearForm3D(norm,
      new V3(start.x, start.y, start.z)
    )
  }
  /**
   * The normal the edge
   */
  def getPlaneNormal: V3 =
    JavaCompat.normalizeJava(direction) ^ uphill

  def length = start.distance(end)

  def direction: V3 =
    new V3(this.end.x, this.end.y, 0) - new V3(this.start.x, this.start.y, 0)

  def distance(ept: V3): Double =
    new Ray3d(start.asV3, end.asV3 - start.asV3).projectSegment(ept) match
      case Some(p) => p.distance(ept)
      case None => Double.MaxValue

  def isCollisionNearHoriz(other: Edge): Boolean =
    linearForm.collide(other.linearForm) match
      case Some(r) => Math.abs(r.direction.z) < 0.001
      case None => false


  /**
   * Do these two edges go in the same direction and are they coliniear.
   * (Are they parallel and go through the same point?)
   */
  def sameDirectedLine(nextL: Edge) = nextL.direction.angle(direction) < 0.01 && Math.abs(getAngle - nextL.getAngle) < 0.01

  def getAngle = angle
  def setAngle(angle: Double): Unit = {
    this.angle = angle
    calculateLinearForm()
  }
  def findLeadingCorners = {
    val out = new mutable.HashSet[Corner]

    for (c <- currentCorners) {
      if (c.nextL == this) out.add(c)
    }
    out
  }


  override def toString = s"Edge( ${start.asV3.toShortString} -> ${end.asV3.toShortString}, ${currentCorners.map(_.asV3).mkString(",")}, $angle, ${if (uphill == null) "null" else uphill.toShortString},)"
}

