package utils.math.planar.algo.straightSkeleton



import java.util
import utils.math.space.V3
import utils.math.space.V3
import javax.vecmath.Vector3d
import org.twak.camp.Output.Face
import org.twak.utils.Cache
import org.twak.utils.IdentityLookup
import org.twak.utils.Triple
import org.twak.utils.collections.ConsecutiveTriples
import org.twak.utils.collections.Loop
import org.twak.utils.collections.LoopL
import org.twak.utils.collections.Loopable
import org.twak.utils.geom.AngleAccumulator
import org.twak.utils.geom.GraphMap

import java.util.{ArrayList, Collection, HashSet, LinkedHashMap, LinkedHashSet, List, Map, Set}


/**
 * @author twak
 */
object Output {
  // marker for horizontal input edges (other edges can also be horizontal...)
  var isCreatedHorizontal = new Tag("horizontal")
  class LoopNormal(var loopl: LoopL[_ <: V3], var norm: Vector3d) {
  }
  class SharedEdge(var start: V3, var end: V3) {
    var left: Output#Face = null
    var right: Output#Face = null
    var cLeft: Loopable[Output.SharedEdge] = null
    var cRight: Loopable[Output.SharedEdge] = null
     val features = new util.HashSet[Tag]
    def getStart(ref: Output#Face): V3 = {
      if (ref eq left) return end
      else if (ref eq right) return start
      null
    }
    def getEnd(ref: Output#Face): V3 = {
      if (ref eq left) return start
      else if (ref eq right) return end
      null
    }
    /**
     * Symetric wrt start, end!
     */
    override def equals(obj: AnyRef): Boolean = {
      if (obj.isInstanceOf[Output.SharedEdge]) {
        val o = obj.asInstanceOf[Output.SharedEdge]
        if (o.start == start) return o.end == end
        else if (o.end == start) return o.start == end
      }
      false
    }
    /**
     * Symetric wrt start, end!
     */
    override def hashCode = {
      var hash = 7
      hash += 71 * (if (this.start != null) this.start.hashCode
      else 0)
      hash += 71 * (if (this.end != null) this.end.hashCode
      else 0)
      hash
    }
    def getOther(ref: Output#Face): Output#Face = {
      if (ref eq left) return right
      else if (ref eq right) return left
      //            System.err.println("bad face in getOther");
      null
    }
    private def setLeft(start: V3, ctxLeft: Loopable[Output.SharedEdge], left: Output#Face): Unit = {
      if (this.start == start) {
        this.left = left
        this.cLeft = ctxLeft
      }
      else if (this.end == start) {
        this.right = left
        this.cRight = ctxLeft
      }
      else throw new Error
    }
    override def toString = "{" + start + " to " + end + "}"
    def getAdjEdge(f: Output#Face, next: Boolean): Output.SharedEdge = if (f eq left) {
      if (cLeft == null) return null
      if (next) cLeft.getNext.get
      else cLeft.getPrev.get
    }
    else {
      if (cRight == null) return null
      if (next) cRight.getNext.get
      else cRight.getPrev.get
    }
    def dir(f: Output#Face): Vector3d = {
      val p = getEnd(f)
      val ps = getStart(f)
      if (p == null || ps == null) return null
      val out = new Vector3d(p)
      out.sub(ps)
      out
    }
    def leftFromStart(x: V3) = if (x == start) left
    else if (x == end) right
    else null
  }
}
class Output(var skeleton: Skeleton) {
  var faces = new util.LinkedHashMap[Corner, Output#Face] // the faces represented by each edge

  var nonSkelFaces = new util.ArrayList[Output.LoopNormal]
  var nonSkelFaces2 = new util.ArrayList[Output.LoopNormal]
  var edges = new IdentityLookup[Output.SharedEdge] // edge ensure that each output edge only exists once

  /**
   * One edge may start in two locations at the same time. To accommodate this, you call
   * newEdge once per new edge, then new Defining Segment for each corner that references that
   * edge.
   *
   * @param startCorner   The corner at the start of the edge at the base of this edge
   * @param aParentCorner A corner whose
   * @param profileFeatures
   */
  def newEdge(e: Edge, aParentLeadingCorner: Corner, profileFeatures: util.Set[Tag]): Unit = {
    val face = new Output#Face
    face.edge = e
    if (profileFeatures != null) face.profile = profileFeatures
    if (aParentLeadingCorner != null) {
      val parentFace = faces.get(aParentLeadingCorner)
      assert(parentFace != null)
      face.parent = parentFace
      skeleton.parent(face, parentFace)
    } // an originator - an edge in the plan

    // we assume that start locations for edges are unique!
    assert(faces.get(e.start) == null)
    e.start.nextL = e // these are always true?! - we rely on them for indexing, below

    e.end.prevL = e
    faces.put(e.start, face)
  }
  /**
   * see newEdge.
   */
  def newDefiningSegment(leadingCorner: Corner): Unit = {
    val face = faces.get(leadingCorner.nextL.start)
    val se = createEdge(leadingCorner, leadingCorner.nextC)
    face.definingSE.add(se)
    se.setLeft(leadingCorner, null, face)
    face.results.add(se.start, se.end)
    se.features.add(Output.isCreatedHorizontal)
    face.definingCorners.add(leadingCorner)
  }
  def addOutputSideTo(a: V3, b: V3, edges: Edge*): Unit = {
    addOutputSideTo(false, a, b, edges)
  }
  def addOutputSideTo(isTop: Boolean, a: V3, b: V3, edges: Edge*): Unit = {
    for (edge <- edges) {
      val c = edge.start // assumption: start of edge will always be a leading corner on the face!

      val f = faces.get(c)
      assert(f != null)
      if (isTop) f.topSE.add(createEdge(a, b))
      // just check those tuple's aren't corners....
      f.results.add(new V3(a), new V3(b))
    }
  } // a.y == b.yequals(b)

  /**
   * Some faces (such as base-plates for globals) can't really be classified nicely.
   * They live here.
   *
   * @param geom
   */
  def addNonSkeletonOutputFace(points: LoopL[_ <: V3], norm: Vector3d): Unit = {
    nonSkelFaces.add(new Output.LoopNormal(points, norm))
  }
  def addNonSkeletonSharedEdges(profileTags: Tag*): Unit = {

    for (ln <- nonSkelFaces) {

      for (loop <- ln.loopl) {
        val f = new Output#Face
        for (t <- profileTags) {
          f.profile.add(t)
        }
        val seloop = new Loop[Output.SharedEdge]
        f.edges.add(seloop)
        f.points = new LoopL[V3]
        val fLoop = new Loop[V3]
        f.points.add(fLoop)

        for (loopable <- loop.loopableIterator) {
          val e = createEdge(loopable.get, loopable.getNext.get)
          fLoop.append(loopable.get)
          if (f.definingSE.isEmpty) {
            f.definingSE.add(e)
            f.edge = new Edge(new Corner(e.start), new Corner(e.end))
            val dir = f.edge.direction
            dir.normalize()
            f.edge.uphill = new Vector3d(-dir.y, dir.x, 0)
            faces.put(new Corner(loopable.get), f)
          }
          e.setLeft(loopable.get, seloop.append(e), f)
        }
      }
    }
  }
  def addNonSkeletonOutputFace2(points: LoopL[V3], norm: Vector3d): Unit = {
    nonSkelFaces2.add(new Output.LoopNormal(points, norm))
  }
  def setParent(neu: Corner, old: Corner): Unit = {
    val nF = faces.get(neu)
    val oF = faces.get(old)
    skeleton.parent(nF, oF)
    nF.parent = oF
  }
  /**
   * Constructs the faces from using the results graph and
   * the points involved with each edge.
   */
  //    public void addNonSkeletonOutputFace ( LoopL<? extends V3> geom )
  //    {
  //        Face f = new Face();
  //
  //        Loopable<? extends V3> eg = geom.get( 0 ).start;
  //
  //        // assume normal is straight down for now;
  //        f.edge = new Edge(eg.get(), eg.getNext().get(), Math.PI);
  //
  //        SharedEdge se = createEdge( f.edge.start, f.edge.end);
  //        f.definingSE.add( se );
  //
  //        se.setLeft( f.edge.start, f );
  //
  //        nonSkelFaces.add( f );
  //
  //        for (Loop<? extends V3> loop : geom)
  //        {
  //            for (Loopable<? extends V3> loopable : loop.loopableIterator())
  //                f.results.add( loopable.get(), loopable.getNext().get() );
  //        }
  //    }
  def calculate(skel: Skeleton): Unit = {
    // todo: reinstante the plan tags somehow
    //        for (Face f : faces.values())
    //            f.plan = skel.getPlanTags( getOriginator( f ) );
    // collect identical edges in different polygons
    edge //todo: labels are not supported

    for (face <- faces.values) {
      //            System.out.println( face.results );
      val notVisited = new util.LinkedHashSet[V3](face.results.map.keySet)
      val faceWithHoles = new LoopL[V3] // first entry here is outer boundary

      face.points = faceWithHoles
      try {
        val edgeStart = face.definingSE.iterator.next.getStart(face)
        //            System.out.println ("results "+face.results); //face.points.count()
        while (!notVisited.isEmpty) {
          // associated face input polygon
          val poly = new Loop[V3]
          //                faceWithHoles.add( poly );
          val isOuter = notVisited.contains(edgeStart)
          val start = if (isOuter) edgeStart
          else notVisited.iterator.next
          var pos = start
          var last = face.results.get(start).get(0) // arb. direction

          var first: V3 = null
          var lastAdded: V3 = null
          val ac = new AngleAccumulator(isOuter, face.edge.getPlaneNormal)
          var count = 0
          pointsInLoop //todo: labels are not supported
          do {
            val choice = face.results.get(pos)
            assert(choice != null)

            for (c <- choice) {
              if ( {
                count += 1; count - 1
              } > 1000) continue edge //todo: continue is not supported // handbrake turn!

              if (!(last == c) && !(pos == c)) {
                if (first == null) first = c
                notVisited.remove(c)
                // remove short edges between the previous corners, and between the current corner and the startstart (bad hack)
                if ((lastAdded == null || lastAdded.distance(c) > 0.01) && ((first eq c) || first.distance(c) > 0.01)) {
                  poly.append(c)
                  ac.add(c)
                  lastAdded = c
                }
                last = pos
                pos = c
                continue pointsInLoop //todo: continue is not supported

              }
            }
            //                    System.out.println( "didn't find faces on " + face.definingSE );
            //                    new GraphMapDebug( face.results );
            //                    System.out.println( face.results );
            continue edge //todo: continue is not supported

          } while (pos ne start)
          // inner loops go counter clockwise
          if (!ac.correctAngle) poly.reverse
          removeStraights(poly)
          // as we remove degenerately small polygons *
          if (poly.count >= 3) faceWithHoles.add(poly)
        }
      } catch {
        case e: Throwable =>

          //                e.printStackTrace();
          continue //todo: continue is not supported

      }
    } //e.toString()

    // * so we remove faces without polygons
    val nullFaces = new util.ArrayList[Output#Face]

    for (f <- faces.values) {
      if (f.points.size <= 0) nullFaces.add(f)
    }
    //        assert (nullFaces.size() == 0); // do something sensible!

    for (f <- faces.values) {
      f.findSharedEdges()
    }
  }
  /**
   * Two parallel faces have become consecutive, remove info about toGo,
   * add to toKeep
   *
   * @toKeep leading corner to keep
   * @toGo leading corner to go
   */
   def merge(toKeep: Corner, toGo: Corner): Unit = {
    val toGoFace = faces.get(toGo.nextL.start)
    val toKeepFace = faces.get(toKeep.nextL.start)
    if (toGoFace == null) {
      System.err.println("three consecutive parallel edges in input?")
      //            Thread.dumpStack();
      return
    }
    toKeepFace.definingSE.addAll(toGoFace.definingSE)
    toKeepFace.results.addEntriesFrom(toGoFace.results)
    toKeepFace.definingCorners.addAll(toGoFace.definingCorners)
    // forward any further face requests to the new one
    //        faces.put (toGo.nextL.start, toKeepFace);
    faces.put(toGo, toKeepFace)
  }
  private def createEdge(start: V3, end: V3) = {
    var newEdge = new Output.SharedEdge(new V3(start), new V3(end))
    newEdge = edges.get(newEdge) // identity lookup - only one edge!

    newEdge
  }
  def getSegmentOriginator = new Cache[Corner, util.Collection[Corner]]() {
    override def get(aCorner: Corner) = {
      var f = faces.get(aCorner.nextL.start)
      while (f.parent != null) f = f.parent
      new util.ArrayList[Corner](f.definingCorners)
    }
    override def create(i: Corner): util.List[Corner] = throw new UnsupportedOperationException("Have overridden get(), shouldn't end up here!")
  }
  def getGreatestGrandParent(f: Output#Face): Output#Face = {
    if (f == null) return null
    while (f.parent != null) f = f.parent
    f
  }
  private def removeStraights(poly: Loop[V3]): Unit = {
    // this will be filtered out later. some of this assumes >= 3 edges.
    if (poly.count < 3) return
    val togo = new util.HashSet[Loopable[V3]]

    for (trip <- new ConsecutiveTriples[Loopable[V3]](poly.loopableIterator, true)) {
      val a = trip.first
      val b = trip.second
      val c = trip.third
      val ab = new Vector3d(b.get)
      val bc = new Vector3d(c.get)
      ab.sub(a.get)
      bc.sub(b.get)
      val angle = ab.angle(bc)
      val small = 0.001
      if (angle < small || angle > Math.PI - small) togo.add(b)
    }

    for (lpb <- togo) {
      poly.remove(lpb)
    }
  }
  class Face {
    // first is outside, others are holes
    var points: LoopL[V3] = null
    var plan = new util.HashSet[Tag]
    var profile = new util.HashSet[Tag]
    // bottom edges
    var definingSE = new util.LinkedHashSet[Output.SharedEdge]
    // defining edges of child (top) edges
    var topSE = new util.LinkedHashSet[Output.SharedEdge]
    // face below us in the skeleton - can be traced back to an originator
    var parent: Output#Face = null
    var results = new GraphMap[V3]
    // a typical edge that defines the plane normal
    var edge: Edge = null
    // subset of results who are horizontal edges and whose nextL are edge, or similar.
    var definingCorners = new util.LinkedHashSet[Corner]
    var edges = new LoopL[Output.SharedEdge]
    def getLoopL = points
    def pointCount = points.count
    // is a defining edges of the above (child) edges
    def isTop(edge: Output.SharedEdge) = topSE.contains(edge)
    // is a defining edge
    def isBottom(edge: Output.SharedEdge) = definingSE.contains(edge)
    // isn't a top or bottom edges
    def isSide(edge: Output.SharedEdge) = !(isTop(edge) || isBottom(edge))
    /**
     * When caculating an offset, we can assume that all edges add a face at every interval
     * this returns the number of faces below this face.
     *
     * @return
     */
    def getParentCount = {
      var count = -1
      var f = this
      while (f != null) {
        count += 1
        f = f.parent
      }
      count
    }
    private def findSharedEdges(): Unit = {
      edges = new LoopL[Output.SharedEdge]

      for (ptLoop <- points) {
        val loop = new Loop[Output.SharedEdge]
        edges.add(loop)
        // start and end points of SharedEdges **aren't** shared

        for (loopable <- ptLoop.loopableIterator) {
          val e = createEdge(loopable.get, loopable.getNext.get)
          e.setLeft(loopable.get, loop.append(e), this)
        }
      }
    }
  }
  def dupeEdgesOnly = {
    val out = new Output(null)
    val fCache = new Cache[Output#Face, Output#Face]() {
      override def create(old: Output#Face) = {
        val face = new Output#Face
        val outGM = new GraphMap[V3]
        outGM.addEntriesFrom(old.results)
        face.results = outGM
        face.parent = if (old.parent == null) null
        else get(old.parent)
        face.edge = new Edge(old.edge.start, old.edge.end)
        face.definingSE = new util.HashSet[Output.SharedEdge]

        for (se <- old.definingSE) {
          val neu = new Output.SharedEdge(se.start, se.end)
          neu.setLeft(se.start, null, face)
          face.definingSE.add(neu)
        }
        face
      }
    }

    for (c <- faces.keySet) {
      out.faces.put(c, fCache.get(faces.get(c)))
    }
    //        for (Face f : nonSkelFaces)
    //            out.nonSkelFaces.add( fCache.get( f ) );
    out
  }

  /**
   * Calling this announces the creation of a new edge. We store it's defining edge,
   * but do not (can not) calculate it's vertices at this time.
   *
   * @param newEdge the edge we're adding
   * @param parent  null if we're an originator, else it's the edge that defines the face
   *                below the one we're creating.
   */
  //    public void addFace ( Edge newEdge, Edge parent, Set<Feature> profileFeatures )
  //    {
  //    }
  //        Face face = new Face();
  //
  //        SharedEdge se= getEdge( face, newEdge.start, newEdge.end );
  //        face.defining.add( se );
  //        se.setLeft( newEdge.start, face );
  //        face.results.add( se.start, se.end );
  //        se.features.add( isInput );
  //
  //        face.edge = newEdge;
  //        if (profileFeatures != null)
  //            face.profile = profileFeatures;
  //
  //        if ( parent != null ) // an originator - an edge in the plan
  //        {
  //            Face parentFace = faces.get( parent );
  //            assert(parentFace != null);
  //            face.parent = parentFace;
  //        }
  //        else
  //            originators.put( newEdge, face );
  //
  //        faces.put( newEdge, face );
  //    }
  //    public Edge getOriginator (Face f)
  //    {
  //        Face parent = f;
  //        while (! originators.containsB( parent ))
  //            parent = f.parent;
  //
  //        return originators.teg( f );
  //    }
}

