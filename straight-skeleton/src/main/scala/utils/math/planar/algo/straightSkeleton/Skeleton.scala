package utils.math.planar.algo.straightSkeleton


import java.util
import java.util.{ArrayList, Collection, Comparator, HashMap, HashSet, LinkedHashMap, LinkedHashSet, List, Map, Set}
import utils.math.space.V3
import utils.math.space.V3
import javax.vecmath.Vector3d
import org.twak.camp.Output.Face
import org.twak.camp.debug.DebugDevice
import org.twak.utils.Cache
import org.twak.utils.collections.CloneConfirmIterator
import org.twak.utils.collections.DHash
import org.twak.utils.collections.Loop
import org.twak.utils.collections.LoopL
import org.twak.utils.collections.Loopable
import org.twak.utils.collections.Loopz
import org.twak.utils.collections.ManyManyMap
import org.twak.utils.collections.MultiMap
import org.twak.utils.collections.SetCorrespondence
import org.twak.utils.geom.LinearForm3D


/**
 * to debug: does it work at all (PointEditor)
 * Offset?
 * Horizontal Edges?
 * Height Collision.processHoriz()?
 *
 * Call
 * <pre>Skeleton skel = new Skeleton( edges );
 * skel.skeleton();</pre>
 *
 * get output from
 * <pre>getOutput</pre>
 *
 * @author twak
 */
object Skeleton {
  class SEC( var start: Corner,  var edge: Edge) {
    end = start.nextC
    prevL = start.prevL
    nextL = end.nextL
     var end: Corner = null
     var nextL: Edge = null
     var prevL: Edge = null
  }
  trait HeresTheArea {
    def heresTheArea(area: Double): Unit
  }
}
class Skeleton {
  var preserveParallel = false
  var volumeMaximising = true
  var liveCorners = new util.LinkedHashSet[Corner]
  var liveEdges = new util.LinkedHashSet[Edge]
  var qu: CollisionQ = null
  var height = 0
  // we store the triplets of faces we've already passed out to stop repeats (insensitive to face order)
  var seen = new util.LinkedHashSet[EdgeCollision]
  // output data
  //    public LoopL<Corner> flatTop = new LoopL<>();
  var output = new Output(this)
  // debug
  var debugCollisionOrder = new util.ArrayList[CoSitedCollision]
  var planFeatures = new util.LinkedHashMap[Edge, util.Set[Tag]]
  // for debugging
  var name = "?"
  // lazy system for refinding all face events. true so we run it once at start
   var refindFaceEvents = true
  def this(corners: LoopL[Corner]) {
    this()
    setup(corners)
  }
  /**
   * @Deprecated
   * @param input list of edges, edges shouldn't be repeated!
   */
  def this(input: LoopL[Edge], javaGenericsAreABigPileOfShite: Boolean) {
    this()
    setupForEdges(input)
  }
  /**
   * @param cap height (flat-topped skeleton) to finish at
   */
  def this(input: LoopL[Corner], cap: Double, javaGenericsAreABigPileOfShite: Boolean) {
    this()
    setup(input)
    capAt(cap)
  }
  /**
   * @Deprecated
   * @param cap height (flat-topped skeleton) to finish at
   */
  def this(input: LoopL[Edge], cap: Double) {
    this()
    setupForEdges(input)
    capAt(cap)
  }
  /**
   * Stop-gap measure to convert loops of edges (BAD!) to loops of corners (GOOD!)
   *
   * @param input
   */
  def setupForEdges(input: LoopL[Edge]): Unit = {
    val corners = new LoopL[Corner]

    for (le <- input) {
      val lc = new Loop[Corner]
      corners.add(lc)

      for (e <- le) {
        lc.append(e.start)
        e.start.nextL = e
        e.end.prevL = e
        e.start.nextC = e.end
        e.end.prevC = e.start
      }
    } //input.count()

    setup(corners) //corners.count()

  }
  /**
   * Sanitize input
   *
   * @param input
   */
  def setup(input: LoopL[Corner]): Unit = {
    // reset all! (not needed...but maybe in future)
    height = 0
    liveCorners.clear()
    liveEdges.clear()
    val allEdges = new MultiMap[Edge, Corner]

    for (c <- input.eIterator) {
      allEdges.put(c.nextL, c)
    } // input.count()

    // combine shared edges into single output faces

    for (e <- allEdges.keySet) {
      e.currentCorners.clear()
      val corners = allEdges.get(e)
      val first = corners.get(0)
      output.newEdge(first.nextL, null, new util.LinkedHashSet[Tag])
      // why don't we need this?
      //            for (Corner c : corners)
      //                output.newDefiningSegment( first );
      // not sure this is right
      for (i <- 1 until corners.size) {
        output.merge(first, corners.get(i))
      }
      liveEdges.add(e)
    } //allEdges.size()


    for (c <- input.eIterator) {
      if (c.z != 0 || c.nextL == null || c.prevL == null) throw new Error("Error in input") // fixme: threading bug with chordatlas under openJDK11 causes npes on nextL?

      output.newDefiningSegment(c)
      liveCorners.add(c)
      c.nextL.currentCorners.add(c)
      c.prevL.currentCorners.add(c)
    }
    qu = new CollisionQ(this) // yay closely coupled classes


    for (e <- allEdges.keySet) {
      e.machine.addEdge(e, this)
    }
    // now all angles are set, find initial set of intersections (will remove corners if parallel enough)
    refindFaceEventsIfNeeded()
    //        qu.dump(); // debug
  }
  /**
   * Execute the skeleton algorithm
   */
  def skeleton(): Unit = {
    validate()
    var he: HeightEvent = null
    var i = 0
    DebugDevice.dump("main " + String.format("%4d", {
      i += 1; i
    }), this)
    while ((he = qu.poll) != null) try {
      if (he.process(this)) {
        height = he.getHeight
        DebugDevice.dump("main " + height + " " + String.format("%4d", {
          i += 1; i
        }), this)
        validate()
      } // business happens here

      refindFaceEventsIfNeeded()
    } catch {
      case t: Throwable =>
        t.printStackTrace()
        if (t.getCause != null) {
          System.out.println(" caused by:")
          t.getCause.printStackTrace()
        }
    }
    DebugDevice.dump("after main " + String.format("%4d", {
      i += 1; i
    }), this)
    // build output polygons from constructed graph
    output.calculate(this)
  }
  /**
   * This method returns a set of edges representing a horizontal slice through the skeleton
   * at the specified height (given that no other events happen bewteen current height and given cap height).
   *
   * Topology assumed final - eg - we take a copy at of the slice at the given height, not processing any more height events
   *
   * Non destructive - this doesn't change the skeleton. This routine is for taking output mid-way through evaluation.
   *
   * All output edges have the same machines as their originators.
   */
  var cornerMap: DHash[Corner, Corner] = null // contains lookup for results (new->old)

  var segmentMap: ManyManyMap[Corner, Corner] = null // contains lookup for results ( old -> new )

  def capCopy(height: Double) = {
    segmentMap = new ManyManyMap[Corner, Corner]
    cornerMap = new DHash[Corner, Corner]
    val ceiling = new LinearForm3D(0, 0, 1, -height)

    for (c <- liveCorners) {
      try {
        var t: V3 = null
        // don't introduce instabilities if height is already as requested.
        if (height == c.z) t = new V3(c)
        else t = ceiling.collide(c.prevL.linearForm, c.nextL.linearForm)
        cornerMap.put(new Corner(t), c)
      } catch {
        case e: RuntimeException =>

          //assume, they're all coincident?
          cornerMap.put(new Corner(c.x, c.y, height), c)
      }
    }
    val edgeCache = new Cache[Corner, Edge]() {
       val lowToHighEdge = new util.HashMap[Edge, Edge]
      override /**
       * @param i the low corner
       */
      def create(i: Corner) = {
        //                Edge cached = lowToHighEdge.get (i.nextL);
        // the following two lines reuse an edge when it is referenced twice. this seems like the better way to do it, but our triangulator can't currently handle two-separate loops of vertices
        //                if (cached != null)
        //                    return cached; // this was one edge, (i.nextL), the raised copy will also be one edge
        val edge = new Edge(cornerMap.teg(i), cornerMap.teg(i.nextC))
        lowToHighEdge.put(i.nextL, edge)
        edge.setAngle(i.nextL.getAngle)
        edge.machine = i.nextL.machine // nextL is null when we have a non root global

        edge
      }
    }
    val out = new LoopL[Corner]
    val workingSet = new util.LinkedHashSet[Corner](liveCorners)
    while (!workingSet.isEmpty) {
      val loop = new Loop[Corner]
      out.add(loop)
      var current = workingSet.iterator.next
      do {
        val s = cornerMap.teg(current)
        val e = cornerMap.teg(current.nextC)
        // one edge may have two segments, but the topology will not change between old and new,
        // so we may store the leading corner to match segments
        segmentMap.addForwards(current, s)
        val edge = edgeCache.get(current)
        loop.append(s)
        s.nextC = e
        e.prevC = s
        s.nextL = edge
        e.prevL = edge
        workingSet.remove(current)
        current = current.nextC
      } while (workingSet.contains(current))
    }
    out
  }
  def getSegmentOriginator = output.getSegmentOriginator
  // when a face is parented, it is flagged here. this allows overriding classes to get even process this information
  def parent(child: Output#Face, parent: Output#Face): Unit = {

    //override me
  } // parent is below (older than) child...

  def capAt(cap: Double): Unit = {
    capAt(cap, null)
  }
  def capAt(cap: Double, hta: Skeleton.HeresTheArea): Unit = {
    qu.add(new HeightEvent() {
      override def getHeight = cap
      override def process(skel: Skeleton) = {
        val capUpdate = new SkeletonCapUpdate(skel)
        val flatTop = capUpdate.getCap(cap)
        capUpdate.update(new LoopL[Corner], new SetCorrespondence[Corner, Corner], new DHash[Corner, Corner])
        val togo = new LoopL[Corner]#Map[V3]() {
          override def map(input: Loopable[Corner]) = new V3(input.get.x, input.get.y, input.get.z)
        }.run
        skel.output.addNonSkeletonOutputFace(togo, new Vector3d(0, 0, 1))
        if (hta != null) hta.heresTheArea(Loopz.area3(togo))
        DebugDevice.dump("post cap dump", skel)
        skel.qu.clearFaceEvents()
        skel.qu.clearOtherEvents()
        true
      }
    })
  }
  def refindAllFaceEventsLater(): Unit = {
    refindFaceEvents = true
  }
  private def refindFaceEventsIfNeeded(): Unit = {
    // on demand
    if (!refindFaceEvents) return
    /**
     * Very expensive part - refind all collisions (including those already processed)
     * MachineEvents remain in their current state
     *
     * should really only be done for those edges that have changed
     */
    // context collects events that must be processed immediately following (eg horizontals...)
    val context = new HeightCollision
    //        qu.clearFaceEvents();

    for (lc <- new CloneConfirmIterator[Corner](liveCorners)) {
      qu.addCorner(lc, context, true)
    }
    // if we are not adding new events (and this isn't adding the input the first time)
    // this shouldn't do anything
    context.processHoriz(this)
  }
  /**
   * Debug!
   */
  def validate(): Unit = {
    if (false) {
      val all = new util.LinkedHashSet[Corner](liveCorners)
      outer //todo: labels are not supported
      while (!all.isEmpty) {
        val start = all.iterator.next
        all.remove(start)
        var next = start
        var count = 0
        do {
          count += 1
          val c = next.nextC
          all.remove(c)
          val e = next.nextL
          try {
            assert(c.nextC.prevC eq c)
            assert(c.prevC.nextC eq c)
            assert(c.prevL eq e)
            assert(c.prevC.nextL eq e)
            //                    assert ( e.start.nextC == e.end );
            //                    assert ( e.end.prevC == e.start ); liveEdges.contains(e)

            for (d <- liveCorners) {
              if ((d.nextL eq e) || (d.prevL eq e)) assert(e.currentCorners.contains(d))
              else assert(!(e.currentCorners.contains(d)))
            }

            for (d <- e.currentCorners) {
              assert(liveCorners.contains(d))
            }
            assert(count < 100)
          } catch {
            case f: AssertionError =>
              System.err.println(" on edge is " + e)
              System.err.println(" validate error on corner " + c + "  on line " + f.getStackTrace(0).getLineNumber)
              f.printStackTrace()
          } finally if (count > 100) continue outer //todo: continue is not supported
            next = c
        } while (next ne start)
      }
    }
  }
  def setPlanTags(edge: Edge, features: util.Set[Tag]): Unit = {
    planFeatures.put(edge, features)
  }
  def getPlanTags(originator: Edge) = planFeatures.get(originator)
  def getHorizontalComparator = new Comparator[Edge]() {
    /**
     * Volume maximizing resolution
     */
    override def compare(o1: Edge, o2: Edge): Int = if (volumeMaximising) return Double.compare(o1.getAngle, o2.getAngle)
    else return Double.compare(o2.getAngle, o1.getAngle)
  }
  def findLoopLive = {
    val out = new LoopL[Corner]
    val togo = new util.HashSet[Corner](liveCorners)
    while (!togo.isEmpty) {
      val loop = new Loop[Corner]
      out.add(loop)
      val start = togo.iterator.next
      var current = start
      var handbrake = 0
      do {
        togo.remove(current)
        loop.append(current)
        current = current.nextC
      } while ((current ne start) && {
        handbrake += 1; handbrake - 1
      } < 1000)
      if (handbrake >= 1000) {
        System.err.println("broken loops in findLiveLoop")
        Thread.dumpStack()
      }
    }
    out //out.count();

  }
}
