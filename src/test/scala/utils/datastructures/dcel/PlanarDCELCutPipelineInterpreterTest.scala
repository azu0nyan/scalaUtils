package utils.datastructures.dcel


import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.AppendedClues._
import utils.datastructures.dcel.DCEL._
import utils.datastructures.dcel.PlanarDCELCutPipeline.{CutChain, Labels}
import utils.math.planar.V2

import java.util.concurrent.atomic.AtomicInteger

class PlanarDCELCutPipelineInterpreterTest extends AnyFunSuite {
  type DATA = DCELData {
    type VertexData = V2
    type HalfEdgeData = Int
    type FaceData = Int
  }
  type Ls = Labels {
    type VertexLabel = Int
    type HalfEdgeLabel = Int
    type FaceLabel = Int
  }

  object Provider extends DCELDataProvider[DATA] {
    val x = new AtomicInteger()
    override def newVertexData(v: V2): V2 = v
    override def newFaceData(edge: HalfEdge[DATA]): Int = x.getAndIncrement()
    override def splitEdgeData(edge: HalfEdge[DATA], data: V2): (Int, Int) = (x.getAndIncrement(), x.getAndIncrement())
    override def newEdgeData(v1: Vertex[DATA], v2: Vertex[DATA]): (Int, Int) = (x.getAndIncrement(), x.getAndIncrement())
  }

  test("Cut simple chain test") {
    val dcel = new PlanarDCEL[DATA](0, x => x)
    /*
         | e1
         |
     e5  |   e4
     ___ |______
         |    /
      e2 |   / e3
         |/
      */
    val op1 = CutChain[DATA, Ls](Seq(V2(0, 200), V2(0, -200), V2(200, 0), V2(-200, 0)),
      Seq(1, 2, 3), Seq(4, 5, 6), Seq(-1, -2, -3, -4))

    val res = PlanarDcelCutPipelineInterpreter.cutPipeline(dcel, Provider, op1)

    assert(res.vertexProduced.size == 5)
    assert(res.halfEdgesProduced.size == 5 * 2)
    assert(res.faceProduced.size == 1)


    val v1 = dcel.getVertex(V2(0, 200)).get
    val v2 = dcel.getVertex(V2(0, -200)).get
    val v3 = dcel.getVertex(V2(200, 0)).get
    val v4 = dcel.getVertex(V2(-200, 0)).get
    val v5 = dcel.getVertex(V2(0, 0)).get
    assert(res.labelToVertex(-1) == Set(v1))
    assert(res.labelToVertex(-2) == Set(v2))
    assert(res.labelToVertex(-3) == Set(v3))
    assert(res.labelToVertex(-4) == Set(v4))

    assert(res.vertexToLabel(v5) == Set())

    assert(res.vertexToLabel(v1) == Set(-1))
    assert(res.vertexToLabel(v2) == Set(-2))
    assert(res.vertexToLabel(v3) == Set(-3))
    assert(res.vertexToLabel(v4) == Set(-4))


    println(dcel.halfEdges.map(e => dcel.asSegment(e)))
    println(v1.edgesWithOriginHere.toSeq.map(e => dcel.asSegment(e)))
    println(v2.edgesWithOriginHere.toSeq.map(e => dcel.asSegment(e)))
    println(v3.edgesWithOriginHere.toSeq.map(e => dcel.asSegment(e)))
    println(v4.edgesWithOriginHere.toSeq.map(e => dcel.asSegment(e)))
    println(v5.edgesWithOriginHere.toSeq.map(e => dcel.asSegment(e)))

    println(v1.edgesWithOriginHere.toSeq.map(e => dcel.asSegment(e.twin)))
    println(v2.edgesWithOriginHere.toSeq.map(e => dcel.asSegment(e.twin)))
    println(v3.edgesWithOriginHere.toSeq.map(e => dcel.asSegment(e.twin)))
    println(v4.edgesWithOriginHere.toSeq.map(e => dcel.asSegment(e.twin)))
    println(v5.edgesWithOriginHere.toSeq.map(e => dcel.asSegment(e.twin)))

    println(v1._incidentEdge)
    println(v2._incidentEdge)
    println(v3._incidentEdge)
    println(v4._incidentEdge)
    println(v5._incidentEdge)


    val e1 = dcel.getEdge(V2(0, 200), V2(0, 0)).get
    val e2 = dcel.getEdge(V2(0, 0), V2(0, -200)).get
    val e3 = dcel.getEdge(V2(0, -200), V2(200, 0)).get
    val e4 = dcel.getEdge(V2(200, 0), V2(0, 0)).get
    val e5 = dcel.getEdge(V2(0, 0), V2(-200, 0)).get

    assert(res.labelToHalfEdge(1) == Set(e1, e2))
    assert(res.labelToHalfEdge(2) == Set(e3))
    assert(res.labelToHalfEdge(3) == Set(e4, e5))

    assert(res.labelToHalfEdge(4) == Set(e1.twin, e2.twin))
    assert(res.labelToHalfEdge(5) == Set(e3.twin))
    assert(res.labelToHalfEdge(6) == Set(e4.twin, e5.twin))



  }
}
