package utils.math.planar.algo

import utils.Logging
import utils.datastructures.containers.BinaryTree.{BinaryTree, EmptyTree}
import utils.datastructures.dcel.DCEL.{DCELData, Face, HalfEdge, Vertex}
import utils.datastructures.dcel.{DCELDataProvider, PlanarDCEL}
import utils.math.planar.{AngleOps, PolygonRegion, TrianglePlanar, V2}
import utils.math.{min, *}

import java.util.logging.Level
import scala.collection.mutable

/**
  * reference
  * De berg
  */
object PolygonTriangulation {

  def innerAngle(prev: V2, v: V2, next: V2): Scalar = (v - next).angleCCW0to2PI(v - prev)

  //Обозначим за ϕ внутренний угол при некоторой вершине и определим далее пять типов вершин, четыре из которых являются поворотными:
  //start вершина — два её соседа лежат ниже её самой и ϕ<π
  //split вершина — два её соседа лежат ниже её самой и ϕ>π
  //end вершина — два её соседа лежат выше её самой и ϕ<π
  //merge вершина — два её соседа лежат выше её самой и ϕ>π
  //regular вершина — не является поворотной, в отличие от остальных, другими словами один её сосед находится выше, а другой ниже её самой.

  def lower(p: V2, q: V2): Boolean = p.y < q.y || (p.y == q.y && p.x > q.x) //??
  def higher(p: V2, q: V2): Boolean = p.y > q.y || (p.y == q.y && p.x < q.x) //??
  sealed trait VType
  case object Start extends VType
  case object Split extends VType
  case object End extends VType
  case object Merge extends VType
  case object Regular extends VType

  def classify(prev: V2, v: V2, next: V2): VType = {
    val inner = innerAngle(prev, v, next)
    if (lower(prev, v) && lower(next, v)) {
      if (inner ~= 0) Split
      else if (inner < PI) Start
      else if (inner > PI) Split
      else Regular //impossible for both lower
    } else if (higher(prev, v) && higher(next, v)) {
      if(inner ~= 0) Merge
      else if (inner < PI) End
      else if (inner > PI) Merge
      else Regular
    } //impossible??
    else Regular
  }

  //  def classifyVertex[VD, HD, FD](d: PlanarDCEL[VD, HD, FD], face: Face[VD, HD, FD], v: Vertex[VD, HD, FD]): VType = {
  //    val prev = v.edgesWithEndHere.find(_.leftFace == face).get.origin
  //    val next = v.edgesWithOriginHere.find(_.leftFace == face).get.origin
  //    classify(d.position(prev), d.position(v), d.position(next))
  //  }

  def monotonePartitionNonHole(polygons: Seq[Seq[V2]]): Seq[Seq[V2]] = {
    object provider extends DCELDataProvider[V2, Unit, Unit] {
      override def splitEdgeData(edge: HalfEdge[V2, Unit, Unit], data: V2): (Unit, Unit) = ((), ())
      override def newVertexData(v: V2): V2 = v
      override def newEdgeData(v1: Vertex[V2, Unit, Unit], v2: Vertex[V2, Unit, Unit]): (Unit, Unit) = ((), ())
      override def newFaceData(edge: HalfEdge[V2, Unit, Unit]): Unit = ()
    }

    val d = new PlanarDCEL[V2, Unit, Unit]((), x => x)

    for (p <- polygons) d.cutPoly(p, provider)

    val res = monotonePartitionNonHolesDCEL(d, provider)
    val result = res.map(_.vertices.map(d.position).toSeq).toSeq
    result
  }

  def monotonePartitionNonHolesDCEL[VD, HD, FD](d: PlanarDCEL[VD, HD, FD], provider: DCELDataProvider[VD, HD, FD]): Seq[Face[VD, HD, FD]] = {
    var res: Seq[Face[VD, HD, FD]] = d.nonHoleFaces
    val sub = d.onNewFace.subscribe(f => res = res :+ f)
    for (f <- d.nonHoleFaces) monotonePartitionDCELFace(d, f, provider)
    d.onNewFace.unSubscribe(sub)
    res
  }


  def triangulateNonHoles(polygons: Seq[Seq[V2]]): Seq[Seq[V2]] = {
    object provider extends DCELDataProvider[V2, Unit, Unit] {
      override def splitEdgeData(edge: HalfEdge[V2, Unit, Unit], data: V2): (Unit, Unit) = ((), ())
      override def newVertexData(v: V2): V2 = v
      override def newEdgeData(v1: Vertex[V2, Unit, Unit], v2: Vertex[V2, Unit, Unit]): (Unit, Unit) = ((), ())
      override def newFaceData(edge: HalfEdge[V2, Unit, Unit]): Unit = ()
    }

    val d = new PlanarDCEL[V2, Unit, Unit]((), x => x)
    for (p <- polygons) d.cutPoly(p, provider)
    val triangulation = triangulateNoneHolesDCEL(d, provider)

    triangulation.map(_.vertices.map(d.position).toSeq).toSeq
  }

  def triangulateNoneHolesDCEL[VD, HD, FD](d: PlanarDCEL[VD, HD, FD], provider: DCELDataProvider[VD, HD, FD]): Seq[Face[VD, HD, FD]] = {
    val (holesSeq, nonHolesSeq) = d.holeNonHoleFaces
    val holes = holesSeq.toSet
    var triangulation = nonHolesSeq

    val sub = d.onNewFace.subscribe(f => triangulation = f +: triangulation)
    for (nh <- nonHolesSeq) monotonePartitionDCELFace(d, nh, provider)

    //continue adding new faces
    for (f <- d.innerFaces.toSeq if !holes.contains(f))
      triangulateMonotoneFace(d, f, provider)

    d.onNewFace.unSubscribe(sub)
    triangulation
  }

  //  def triangulateMonotonePartitionedDCEL[VD, HD, FD](d: PlanarDCEL[VD, HD, FD], provider: DCELDataProvider[VD, HD, FD]): Unit = {
  //    for (face <- d.innerFaces) triangulateMonotone(d, face, provider)
  //  }

  //Actual working functions

  def monotonePartitionDCELFace[VD, HD, FD](dcel: PlanarDCEL[VD, HD, FD], face: Face[VD, HD, FD], provider: DCELDataProvider[VD, HD, FD]): Seq[Face[VD, HD, FD]] = {
    var curY = 0d
    var res: Seq[Face[VD, HD, FD]] = Seq(face)
    val sub = dcel.onNewFace.subscribe(f => res = f +: res)

    println(s"Monotone Partitioning s${face.vertices.toSeq}")


    def queueLt(p: V2, q: V2): Boolean = p.y < q.y || (p.y == q.y && p.x > q.x) //??
    //using halfEdges instead of vertices to prevent duplicate vertices in chains
    val q = new mutable.PriorityQueue[HalfEdge[VD, HD, FD]]()(Ordering.fromLessThan((v1, v2) => queueLt(dcel.position(v1.origin), dcel.position(v2.origin))))
    for (v <- face.edges) q += v

    //todo check
    implicit val heOrd: Ordering[HalfEdge[VD, HD, FD]] = Ordering.fromLessThan { (he1, he2) =>
      val he1Seg = dcel.asSegment(he1)
      val x1 = he1Seg.xFromY(curY)
      val he2Seg = dcel.asSegment(he2)
      val x2 = he2Seg.xFromY(curY)
      if (x1.nonEmpty && x2.nonEmpty) x1.get < x2.get
      else if (x1.nonEmpty) x1.get < min(he2Seg.start.x, he2Seg.end.x)
      else if (x2.nonEmpty) min(he1Seg.start.x, he1Seg.end.x) < x2.get
      else min(he1Seg.start.x, he1Seg.end.x) < min(he2Seg.start.x, he2Seg.end.x)
    }
    def xleq(x: Scalar): HalfEdge[VD, HD, FD] => Boolean = he => dcel.asSegment(he).xFromY(curY) match {
      case Some(value) => value <= x
      case None => min(dcel.asSegment(he).start.x, dcel.asSegment(he).end.x) <= x
    }

    var xStructure: BinaryTree[HalfEdge[VD, HD, FD]] = EmptyTree
    val helper: mutable.Map[HalfEdge[VD, HD, FD], HalfEdge[VD, HD, FD]] = mutable.Map()

    //bake since can be changed during algo(is it important??)
    val prevEdge: Map[HalfEdge[VD, HD, FD], HalfEdge[VD, HD, FD]] = face.edges.map(he => (he, he.prev)).toMap //face.vertices.map(v => (v, v.edgesWithEndHere.find(_.leftFace == face).get)).toMap
    //    val nextEdge: Map[HalfEdge[VD, HD, FD], HalfEdge[VD, HD, FD]] = face.edges.map(he => (he, he.next)).toMap //face.vertices.map(v => (v, v.edgesWithOriginHere.find(_.leftFace == face).get)).toMap
    //    val prevVertex: Map[Vertex[VD, HD, FD], Vertex[VD, HD, FD]] = face.vertices.map(v => (v, prevEdge(v).origin)).toMap
    //    val nextVertex: Map[Vertex[VD, HD, FD], Vertex[VD, HD, FD]] = face.vertices.map(v => (v, nextEdge(v).ending)).toMap
    val classifyInner: Map[HalfEdge[VD, HD, FD], VType] =
    face.edges.map(vHe => (vHe, classify(dcel.position(prevEdge(vHe).origin), dcel.position(vHe.origin), dcel.position(vHe.ending)))).toMap

    while (q.nonEmpty) {
      val xst = xStructure.elements
      val cur = q.dequeue()
      curY = dcel.position(cur.origin).y
      //      println(s"${dcel.position(cur)} ${classifyInner(cur)}")
      //      println(xStructure)
      classifyInner(cur) match {
        case Start => handleStart(cur)
        case Split => handleSplit(cur)
        case End => handleEnd(cur)
        case Merge => handleMerge(cur)
        case Regular => handleRegular(cur)
      }
    }


    //vi = v.origin
    //ei =v
    def handleStart(v: HalfEdge[VD, HD, FD]): Unit = {
      // Insert ei in T and set helper(ei ) to vi.
      xStructure = xStructure.add(v)
      helper += v -> v
    }

    def handleSplit(v: HalfEdge[VD, HD, FD]): Unit = {
      val xst = xStructure.elements
      //      Search ej in T
      val ej = xStructure.maximumSatisfiesCondition(xleq(dcel.position(v.origin).x)).get
      //        Insert edge(vi, helper(ej)) in D
      val toConnect = helper(ej)
      dcel.connectVerticesUnsafe(v.origin, toConnect.origin, provider)
      helper += ej -> v
      xStructure = xStructure.add(v)
      helper += v -> v
    }

    def handleEnd(v: HalfEdge[VD, HD, FD]): Unit = {
      if (classifyInner(helper(prevEdge(v))) == Merge) {
        dcel.connectVerticesUnsafe(v.origin, helper(prevEdge(v)).origin, provider)
      }
      xStructure = xStructure.remove(prevEdge(v))
    }

    def handleMerge(v: HalfEdge[VD, HD, FD]): Unit = {
      val toConnect = helper(prevEdge(v))
      if (classifyInner(toConnect) == Merge) {
        dcel.connectVerticesUnsafe(v.origin, toConnect.origin, provider)
      }
      xStructure = xStructure.remove(prevEdge(v))

      val ej = xStructure.maximumSatisfiesCondition(xleq(dcel.position(v.origin).x)).get
      if (classifyInner(helper(ej)) == Merge) {
        dcel.connectVerticesUnsafe(v.origin, helper(ej).origin, provider)
      }
      helper += ej -> v
    }

    //imagine slight CW rotation
    //only for regular vertices
    def liesToTheRight(p: V2, v: V2): Boolean = {
      v.y < p.y || (v.y == p.y && p.x < v.x)
    }
    def handleRegular(v: HalfEdge[VD, HD, FD]): Unit = {
      //if interior of P lies to the right of v
      if (liesToTheRight(dcel.position(prevEdge(v).origin), dcel.position(v.origin))) {
        if (classifyInner(helper(prevEdge(v))) == Merge) {
          dcel.connectVerticesUnsafe(v.origin, helper(prevEdge(v)).origin, provider)
        }
        xStructure = xStructure.remove(prevEdge(v))
        xStructure = xStructure.add(v)
        helper += v -> v
      } else {
        val x = xStructure.elements
        val ej = xStructure.maximumSatisfiesCondition(xleq(dcel.position(v.origin).x)).get
        if (classifyInner(helper(ej)) == Merge) {
          dcel.connectVerticesUnsafe(v.origin, helper(ej).origin, provider)
          helper += ej -> v
        }
      }

    }


    dcel.onNewFace.unSubscribe(sub)
    res
  }


  def triangulateMonotoneFace[VD, HD, FD](d: PlanarDCEL[VD, HD, FD], face: Face[VD, HD, FD], provider: DCELDataProvider[VD, HD, FD]): Seq[Face[VD, HD, FD]] = {
    implicit def vToV2(v: Vertex[VD, HD, FD]): V2 = d.position(v)
    val vs = face.vertices.toSeq
    if (vs.size > 3) {
      var res = Seq(face)
      val sub = d.onNewFace.subscribe(n => res = n +: res)

      /** *_ start
        * / |\
        * /    \
        * |_      \
        * *        *
        * \      /\
        * _\  /
        * *   end
        * //rotating slightly CW
        */

      val max = vs.maxBy(v => (v.y, -v.x))
      val min = vs.minBy(v => (v.y, -v.x))
      val maxI = vs.indexOf(max)
      val minI = vs.indexOf(min)
      val leftVertices = if (maxI < minI) vs.slice(maxI, minI).toSet
      else vs.slice(maxI, vs.size).toSet | vs.slice(0, minI).toSet

      //      val leftVertices = (vs ++ vs).dropWhile(classifyVertex(d, face, _) != Start).takeWhile(classifyVertex(d, face, _) != End).toSet
      //      val rightVertices = (vs ++ vs).dropWhile(classifyVertex(d, face, _) != End).takeWhile(classifyVertex(d, face, _) != Start).toSet
      val u = vs.sortBy(v => {
        val p = d.position(v);
        (-p.y, p.x)
      }).toIndexedSeq

      println("vs: " + vs.map(_.toProduct).toString())
      println("u: " + u.map(d.position).toString())
      println(minI.toString)
      println(maxI.toString)
      println("leftVertices: " + leftVertices.map(_.toProduct).toString())

      //Initialize an empty stack S, and push u1 and u2 onto it.
      val s = mutable.Stack[Vertex[VD, HD, FD]]()
      s.push(u(0))
      s.push(u(1))
      for (j <- 2 to (u.size - 2)) {
        val slc = leftVertices.contains(s.top)
        val ujlc = leftVertices.contains(u(j))
        println(s.map(_.toProduct))
        println(s"${s.top.toProduct} ${u(j).toProduct} $slc $ujlc")
        // if u_j and the vertex on top of S are on different chains
        if (slc != ujlc) {
          // then Pop all vertices from S.
          while (s.nonEmpty) {
            val cur = s.pop()
            //Insert into D a diagonal from u_j to each popped vertex, except the last one.
            if (s.nonEmpty) d.connectVerticesUnsafe(u(j), cur, provider)
          }
          //Push u_{j−1} and u_j onto S
          s.push(u(j - 1))
          s.push(u(j))
        } else {
          // Pop one vertex from S.
          var lastPopped = s.pop()
          //Pop the other vertices from S as long as the diagonals from u_j to them are inside P.
          println(s"${u(j).toProduct} ${lastPopped.toProduct} ${s.top.toProduct} ${AngleOps.isCCW(u(j), lastPopped, s.top)}")
          while (s.nonEmpty && TrianglePlanar(u(j), lastPopped, s.top).nonDegenerate && (ujlc != AngleOps.isCCW(u(j), lastPopped, s.top))) {
            lastPopped = s.pop()
            // Insert these diagonals into D
            d.connectVerticesUnsafe(u(j), lastPopped, provider)
          }
          //Push the last vertex that has been popped back onto S.
          s.push(lastPopped)
          // Push u_j onto S.
          s.push(u(j))
        }
      }
      //Add diagonals from u_n to all stack vertices except the first and the last one.
      s.pop()
      while (s.size >= 2) {
        val c = s.pop()
        d.connectVerticesUnsafe(u.last, c, provider)
      }

      d.onNewFace.unSubscribe(sub)
      res
    } else Seq(face)
  }
}
