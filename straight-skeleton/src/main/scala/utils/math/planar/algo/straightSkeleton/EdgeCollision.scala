package utils.math.planar.algo.straightSkeleton


import utils.math.planar.algo.straightSkeleton.math.LinearForm3D
import utils.math.space.V3

/**
 *
 * @author twak
 */
object EdgeCollision {
  def processConsecutive(loc: V3, a: Corner, b: Corner, skel: Skeleton): Unit = {
    // add line b -> loc
    skel.output.addOutputSideTo(b.asV3, loc, b.prevL, b.nextL)
    // remove b from edge's map
    a.nextL.currentCorners.remove(b)
    b.nextL.currentCorners.remove(b)
    skel.liveCorners.remove(b)
  }
  def processJump(loc: V3, a: Corner, an: Corner, b: Corner, skel: Skeleton, hc: HeightCollision): Unit = {
    val ab = new Corner(loc.x, loc.y, loc.z)
    ab.prevL = b.nextL
    ab.prevL.currentCorners.add(ab)
    ab.nextL = a.nextL
    ab.nextL.currentCorners.add(ab)
    // take's A's place in the loop
    an.prevC = ab // where it breaks down without an...

    b.nextC = ab
    ab.prevC = b
    ab.nextC = an // ..and here

    skel.liveCorners.add(ab)
    // check for new collisions
    skel.qu.addCorner(ab, hc) // we could optimize and move to after having removed from liveEdges? (commented out - above)

  }
  /**
   * @return the loop corner whose nextL points to the given edge and whose
   *         bisectors for the edge contain the collision.
   */
  def bisectorsBound(first: Corner, collision: V3, skel: Skeleton) = {
    // the two edges that form the bisector with this edge
    var prev = first.prevL.linearForm // clone not needed now?

    var next = first.nextC.nextL.linearForm
    var pDist = prev.pointDistance(collision)
    var nDist = next.pointDistance(collision)
    val prevDot = prev.normal ** first.nextL.direction
    val nextDot = next.normal ** first.nextL.direction
    // depending on if the angle is obtuse or reflex, we'll need to flip the normals
    // to the convention that a point with a positive plane distance is on the correct side of both bisecting planes
    if (prevDot < 0) pDist = -pDist // should only be 0 if two edges are parallel!

    if (nextDot > 0) nDist = -nDist
    if (first.nextC.nextL.uphill == first.nextL.uphill) {
      val dir = -JavaCompat.normalizeJava(first.nextL.direction)
      next = new LinearForm3D(dir, first.nextC.asV3)
      nDist = next.pointDistance(collision)
    }
    if (first.prevL.uphill == first.nextL.uphill) {
      val dir = JavaCompat.normalizeJava(first.prevL.direction)
      prev = new LinearForm3D(dir, first.asV3)
      pDist = prev.pointDistance(collision)
    }
    // important constant - must prefer to accept rather than "leak" a collision
    val c = -0.0001
    pDist >= c && nDist >= c // a bit of slack!

  }

  def findCorner(in: Edge, collision: V3, skel: Skeleton): Corner = {

    for (lc <- in.currentCorners) {
      if (lc.nextL eq in) {
        // the two edges that form the bisector with this edge
        val prev = lc.prevC.nextL.linearForm.clone // clone not needed now?

        val next = lc.nextC.nextL.linearForm.clone
        var pDist = prev.pointDistance(collision)
        var nDist = next.pointDistance(collision)
        val prevDot = prev.normal ** in.direction
        val nextDot = next.normal ** in.direction
        // depending on if the angle is obtuse or reflex, we'll need to flip the normals
        // to the convention that a point with a positive plane distance is on the correct side of both bisecting planes
        if (prevDot < 0) pDist = -pDist // should only be 0 if two edges are parallel!

        if (nextDot > 0) nDist = -nDist
        //                    if (lc.nextC.nextL.uphill.equals (in.uphill) )
        //                    {
        //                    	Vector3d dir = in.direction();
        //                    	dir.normalize();
        //                    	dir.negate();
        //                    	next = new LinearForm3D( dir, lc.nextC );
        //                    	nDist = next.pointDistance( collision );
        //                    }
        //
        //                    if (lc.prevC.nextL.uphill.equals (in.uphill) ) {
        //                    	Vector3d dir = in.direction();
        //                    	dir.normalize();
        //                    	prev = new LinearForm3D(dir, lc.prevC );
        //                    	pDist = prev.pointDistance( collision );
        //                    }
        // important constant - must prefer to accept rather than "leak" a collision
        val c = -0.0001
        if (pDist >= c && nDist >= c) return lc // a bit of slack!

      }
    }
    null // no candidates

  }
}

class EdgeCollision(var loc: Option[V3], var a: Edge, var b: Edge, var c: Edge) extends HeightEvent {
  var debugInfinite = false
  override def equals(obj: Any): Boolean = {
    if (obj.isInstanceOf[EdgeCollision]) {
      val other = obj.asInstanceOf[EdgeCollision]
      return (a == other.a && ((b == other.b && c == other.c) || (b == other.c && c == other.b))) || (a == other.b && ((b == other.a && c == other.c) || (b == other.c && c == other.a))) || (a == other.c && ((b == other.a && c == other.b) || (b == other.b && c == other.a)))
    }
    false
  }
  /**
   * Hash is agnostic to which edge is in a, b and c
   *
   * @return
   */
  override def hashCode = {
    var hash = 3
    hash += (if (this.a != null) this.a.hashCode
    else 0)
    hash += (if (this.b != null) this.b.hashCode
    else 0)
    hash += (if (this.c != null) this.c.hashCode
    else 0)
    hash * 31
  }
  override def getHeight = loc match
    case Some(v) => v.z
    case None => throw new Exception("EdgeCollision has no height")
  /**
   * Three way collisions are delt with in CoSitedCollision
   */
  override def process(skel: Skeleton): Boolean = throw new Error

  override def toString = "EdgeCollision(" + loc.map(_.toShortString).getOrElse("None") + ":" + a + "," + b + "," + c + ")"
}

