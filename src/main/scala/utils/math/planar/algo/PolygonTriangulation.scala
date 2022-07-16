package utils.math.planar.algo

import utils.datastructures.containers.BinaryTree.{BinaryTree, EmptyTree}
import utils.datastructures.containers.ThreadedAVLTree.ThreadedAVLTree
import utils.datastructures.dcel.DCEL.{DCELData, HalfEdge, Vertex}
import utils.datastructures.dcel.{DCELDataProvider, PlanarDCEL}
import utils.math.planar.{PolygonRegion, V2}
import utils.math._

import scala.collection.mutable

/**
  * reference
  * https://neerc.ifmo.ru/wiki/index.php?title=%D0%A2%D1%80%D0%B8%D0%B0%D0%BD%D0%B3%D1%83%D0%BB%D1%8F%D1%86%D0%B8%D1%8F_%D0%BF%D0%BE%D0%BB%D0%B8%D0%B3%D0%BE%D0%BD%D0%BE%D0%B2_(%D1%83%D1%88%D0%BD%D0%B0%D1%8F_%2B_%D0%BC%D0%BE%D0%BD%D0%BE%D1%82%D0%BE%D0%BD%D0%BD%D0%B0%D1%8F)
  */
object PolygonTriangulation {

  def innerAngle(prev: V2, v: V2, next: V2): Scalar = (v - next).angleCCW0to2PI(v - prev)

  //Обозначим за ϕ внутренний угол при некоторой вершине и определим далее пять типов вершин, четыре из которых являются поворотными:
  //start вершина — два её соседа лежат ниже её самой и ϕ<π
  //split вершина — два её соседа лежат ниже её самой и ϕ>π
  //end вершина — два её соседа лежат выше её самой и ϕ<π
  //merge вершина — два её соседа лежат выше её самой и ϕ>π
  //regular вершина — не является поворотной, в отличие от остальных, другими словами один её сосед находится выше, а другой ниже её самой.

  def lower(p: V2, q: V2): Boolean = p.y < q.y || (p.y == q.y && p.x < q.x) //??
  sealed trait VType
  case object Start extends VType
  case object Split extends VType
  case object End extends VType
  case object Merge extends VType
  case object Regular extends VType

  def classify(prev: V2, v: V2, next: V2): VType = {
    val l1 = lower(prev, v)
    val l2 = lower(next, v)
    val inner = innerAngle(prev, v, next)
    if (l1 && l2)
      if (inner < PI) Start
      else if (inner > PI) Split
      else Regular //impossible???
    else if (!l1 && !l2)
      if (inner < PI) End
      else if (inner > PI) Merge
      else Regular //impossible??
    else Regular
  }


  def monotonePartition(p: PolygonRegion) = {
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
    d.cutPoly(p.vertices, provider)

    monotonePartitionDCEL[DATA](d, provider)
  }

  def monotonePartitionDCEL[D <: DCELData](dcel: PlanarDCEL[D], provider: DCELDataProvider[D]) = {

    var curY = 0d

    def queueLt(p: V2, q: V2): Boolean = p.y < q.y || (p.y == q.y && p.x < q.x) //??
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
    val closestLt: (HalfEdge[D], Scalar) => Boolean = (he, x) => dcel.asSegment(he).xFromY(curY) match {
      case Some(value) => value < x
      case None => min(dcel.asSegment(he).start.x, dcel.asSegment(he).end.x) < x
    }

    var t: BinaryTree[HalfEdge[D]] = EmptyTree
    val helper: mutable.Map[HalfEdge[D], Vertex[D]] = mutable.Map()

    val prevEdge: Map[Vertex[D], HalfEdge[D]] = dcel.vertices.map(v => (v, v.edgesWithEndHere.find(_.leftFace != dcel.outerFace).get)).toMap
    val nextEdge: Map[Vertex[D], HalfEdge[D]] = dcel.vertices.map(v => (v, v.edgesWithOriginHere.find(_.leftFace != dcel.outerFace).get)).toMap
    val prevVertex: Map[Vertex[D], Vertex[D]] = dcel.vertices.map(v => (v, prevEdge(v).origin)).toMap
    val nextVertex: Map[Vertex[D], Vertex[D]] = dcel.vertices.map(v => (v, nextEdge(v).ending)).toMap
    val classifyInner: Map[Vertex[D], VType] = dcel.vertices.map(v => (v, classify(dcel.position(prevVertex(v)), dcel.position(v), dcel.position(nextVertex(v))))).toMap

    while (q.nonEmpty) {
      val cur = q.dequeue()
      curY = dcel.position(cur).y

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
      t = t.add(next)
      helper += next -> v
    }

    def handleSplit(v: Vertex[D]): Unit = {
      //      Search ej in T
      val ej = t.closestLessB( dcel.position(v).x, closestLt).get
      //        Insert edge(vi, helper(ej)) in D
      val toConnect = helper(ej)
      dcel.addEdgeUnsafe(v, toConnect, provider)
      helper += ej -> v
      t = t.add( nextEdge(v))
      helper += nextEdge(v) -> v
    }

    def handleEnd(v: Vertex[D]): Unit = {
      if (classifyInner(prevVertex(v)) == Merge) {
        dcel.addEdgeUnsafe(v, helper(prevEdge(v)), provider)
      }
      t = t.remove( prevEdge(v))
    }

    def handleMerge(v: Vertex[D]): Unit = {
      val toConnect = helper(prevEdge(v))
      if (classifyInner(toConnect) == Merge) {
        dcel.addEdgeUnsafe(v, toConnect, provider)
      }
      t = t.remove( prevEdge(v))

      val ej = t.closestLessB( dcel.position(v).x, closestLt).get
      if (classifyInner(helper(ej)) == Merge) {
        dcel.addEdgeUnsafe(v, helper(ej), provider)
      }
      helper += ej -> v
    }

    def handleRegular(v: Vertex[D]): Unit = {
      //if interior of P lies to the right of v
      if (dcel.position(nextVertex(v)).y > dcel.position(v).y) {
        if (classifyInner(helper(prevEdge(v))) == Merge) {
          dcel.addEdgeUnsafe(v, helper(prevEdge(v)), provider)
        }
        t = t.remove( prevEdge(v))
        t = t.add( nextEdge(v))
        helper += nextEdge(v) -> v
      } else {
        val ej = t.closestLessB( dcel.position(v).x, closestLt).get
        if (classifyInner(helper(ej)) == Merge) {
          dcel.addEdgeUnsafe(v, helper(ej), provider)
          helper += ej -> v
        }
      }

    }

  }
}
