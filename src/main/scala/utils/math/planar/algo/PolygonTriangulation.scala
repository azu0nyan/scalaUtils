package utils.math.planar.algo

import utils.datastructures.containers.BinaryTree.{BinaryTree, EmptyTree}
import utils.datastructures.containers.ThreadedAVLTree.ThreadedAVLTree
import utils.datastructures.dcel.DCEL.{DCELData, Face, HalfEdge, Vertex}
import utils.datastructures.dcel.{DCELDataProvider, PlanarDCEL}
import utils.math.planar.{PolygonRegion, V2}
import utils.math._

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
    if (lower(prev, v) && lower(next, v))
      if (inner < PI) Start
      else if (inner > PI) Split
      else Regular //impossible???
    else if (higher(prev, v) && higher(next, v))
      if (inner < PI) End
      else if (inner > PI) Merge
      else Regular //impossible??
    else Regular
  }

  def classifyVertex[D <: DCELData](d: PlanarDCEL[D], face: Face[D], v: Vertex[D]): VType = {
    val prev = v.edgesWithEndHere.find(_.leftFace == face).get.origin
    val next = v.edgesWithOriginHere.find(_.leftFace == face).get.origin
    classify(d.position(prev), d.position(v), d.position(next))
  }

  def monotonePartition(polygons: Seq[Seq[V2]]): Seq[Seq[V2]] = {
    type DATA = DCELData {
      type VertexData = V2
      type HalfEdgeData = Unit
      type FaceData = Unit
    }
    object provider extends DCELDataProvider[DATA] {
      override def splitEdgeData(edge: HalfEdge[DATA], data: V2): (Unit, Unit) = ((), ())
      override def newVertexData(v: V2): V2 = v
      override def newEdgeData(v1: Vertex[DATA], v2: Vertex[DATA]): (Unit, Unit) = ((), ())
      override def newFaceData(edge: HalfEdge[DATA]): Unit = ()
    }

    val d = new PlanarDCEL[DATA]((), x => x)
    for (p <- polygons) d.cutPoly(p, provider)

    monotonePartitionDCEL[DATA](d, provider)
    d.innerFaces.map(_.vertices.map(d.position).toSeq).toSeq
  }

  def monotonePartitionDCEL[D <: DCELData](dcel: PlanarDCEL[D], provider: DCELDataProvider[D]) = {

    var curY = 0d

    def queueLt(p: V2, q: V2): Boolean = p.y < q.y || (p.y == q.y && p.x > q.x) //??
    val q = new mutable.PriorityQueue[Vertex[D]]()(Ordering.fromLessThan((v1, v2) => queueLt(dcel.position(v1), dcel.position(v2))))
    for (v <- dcel.vertices) q += v

    //todo check
    implicit val heOrd: Ordering[HalfEdge[D]] = Ordering.fromLessThan { (he1, he2) =>
      val he1Seg = dcel.asSegment(he1)
      val x1 = he1Seg.xFromY(curY)
      val he2Seg = dcel.asSegment(he2)
      val x2 = he2Seg.xFromY(curY)
      if (x1.nonEmpty && x2.nonEmpty) x1.get < x2.get
      else if (x1.nonEmpty) x1.get < min(he2Seg.start.x, he2Seg.end.x)
      else if (x2.nonEmpty) min(he1Seg.start.x, he1Seg.end.x) < x2.get
      else min(he1Seg.start.x, he1Seg.end.x) < min(he2Seg.start.x, he2Seg.end.x)
    }
    def xleq(x: Scalar): HalfEdge[D] => Boolean = he => dcel.asSegment(he).xFromY(curY) match {
      case Some(value) => value <= x
      case None => min(dcel.asSegment(he).start.x, dcel.asSegment(he).end.x) <= x
    }

    var xStructure: BinaryTree[HalfEdge[D]] = EmptyTree
    val helper: mutable.Map[HalfEdge[D], Vertex[D]] = mutable.Map()

    val prevEdge: Map[Vertex[D], HalfEdge[D]] = dcel.vertices.map(v => (v, v.edgesWithEndHere.find(_.leftFace != dcel.outerFace).get)).toMap
    val nextEdge: Map[Vertex[D], HalfEdge[D]] = dcel.vertices.map(v => (v, v.edgesWithOriginHere.find(_.leftFace != dcel.outerFace).get)).toMap
    val prevVertex: Map[Vertex[D], Vertex[D]] = dcel.vertices.map(v => (v, prevEdge(v).origin)).toMap
    val nextVertex: Map[Vertex[D], Vertex[D]] = dcel.vertices.map(v => (v, nextEdge(v).ending)).toMap
    val classifyInner: Map[Vertex[D], VType] = dcel.vertices.map(v => (v, classify(dcel.position(prevVertex(v)), dcel.position(v), dcel.position(nextVertex(v))))).toMap

    while (q.nonEmpty) {
      val cur = q.dequeue()
      curY = dcel.position(cur).y
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

    def handleStart(v: Vertex[D]): Unit = {
      val next = nextEdge(v)
      xStructure = xStructure.add(next)
      helper += next -> v
    }

    def handleSplit(v: Vertex[D]): Unit = {
      //      Search ej in T
      val ej = xStructure.maximumSatisfiesCondition(xleq(dcel.position(v).x)).get
      //        Insert edge(vi, helper(ej)) in D
      val toConnect = helper(ej)
      dcel.connectVerticesUnsafe(v, toConnect, provider)
      helper += ej -> v
      xStructure = xStructure.add(nextEdge(v))
      helper += nextEdge(v) -> v
    }

    def handleEnd(v: Vertex[D]): Unit = {
      if (classifyInner(prevVertex(v)) == Merge) {
        dcel.connectVerticesUnsafe(v, helper(prevEdge(v)), provider)
      }
      xStructure = xStructure.remove(prevEdge(v))
    }

    def handleMerge(v: Vertex[D]): Unit = {
      val toConnect = helper(prevEdge(v))
      if (classifyInner(toConnect) == Merge) {
        dcel.connectVerticesUnsafe(v, toConnect, provider)
      }
      xStructure = xStructure.remove(prevEdge(v))

      val ej = xStructure.maximumSatisfiesCondition(xleq(dcel.position(v).x)).get
      if (classifyInner(helper(ej)) == Merge) {
        dcel.connectVerticesUnsafe(v, helper(ej), provider)
      }
      helper += ej -> v
    }

    //imagine slight CW rotation
    //only for regular vertices
    def liesToTheRight(p: V2, v: V2): Boolean = {
      v.y < p.y || (v.y == p.y && p.x < v.x)
    }
    def handleRegular(v: Vertex[D]): Unit = {
      //if interior of P lies to the right of v
      if (liesToTheRight(dcel.position(prevVertex(v)), dcel.position(v))) {
        if (classifyInner(helper(prevEdge(v))) == Merge) {
          dcel.connectVerticesUnsafe(v, helper(prevEdge(v)), provider)
        }
        xStructure = xStructure.remove(prevEdge(v))
        xStructure = xStructure.add(nextEdge(v))
        helper += nextEdge(v) -> v
      } else {
        val ej = xStructure.maximumSatisfiesCondition(xleq(dcel.position(v).x)).get
        if (classifyInner(helper(ej)) == Merge) {
          dcel.connectVerticesUnsafe(v, helper(ej), provider)
          helper += ej -> v
        }
      }

    }

  }

  def triangulateMonotone[D <: DCELData](d: PlanarDCEL[D], face: Face[D]): Unit = {
    val vs = face.vertices.toSeq
    if (vs.size > 3) {

      /** *_ start
        * / |\
        * /    \
        * |_      \
        * *        *
        * \      /\
        * _\  /
        * *   end
        */

      val leftVertices = (vs ++ vs).dropWhile(classifyVertex(d, face, _) != Start).takeWhile(classifyVertex(d, face, _) != End).toSet
//      val rightVertices = (vs ++ vs).dropWhile(classifyVertex(d, face, _) != End).takeWhile(classifyVertex(d, face, _) != Start).toSet

      val u = vs.sortBy(v => {
        val p = d.position(v);
        (-p.y, -p.x)
      }).toIndexedSeq
      val s = mutable.Stack[Vertex[D]]()
      s += u(0)
      s += u(1)
      for(j <- 2 to (u.size- 2)){
        val slc =  leftVertices.contains(s.top)
        val jlc = leftVertices.contains(u(j))
        if(slc != jlc) { //if u_j and the vertex on top of S are on different chains

        }
      }

    }
  }
}
