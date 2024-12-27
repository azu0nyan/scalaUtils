package utils.datastructures.dcel


import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.AppendedClues._
import utils.datastructures.dcel.DCEL._
import utils.datastructures.dcel.PlanarDCELCutPipeline.{CutChain, CutPoly, CuttingContext, Labels, MergeFaces, StepsSeq, TraceSegmentAtAngle}
import utils.datastructures.spatial.AARectangle
import utils.math.planar.{PolygonRegion, V2}
import utils.math._

import java.util.concurrent.atomic.AtomicInteger

class PlanarDCELCutPipelineInterpreterTest extends AnyFunSuite {
  type DATA = DCELData {
    type VertexData = V2
    type HalfEdgeData = Int
    type FaceData = Int
  }
  type LABELS = Labels {
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
    val op1 = CutChain[DATA, LABELS](Seq(V2(0, 200), V2(0, -200), V2(200, 0), V2(-200, 0)),
      Seq(1, 2, 3), Seq(4, 5, 6), Seq(-1, -2, -3, -4))

    val res = PlanarDcelCutPipelineInterpreter.cutPipeline(dcel, Provider, op1)
    //println(dcel.toLongSting)
    dcel.sanityCheck()
    dcel.planarSanityCheck()

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


//    println(dcel.halfEdges.mkString("\n"))
//
//    println(v1.edgesWithOriginHere.toSeq)
//    println(v2.edgesWithOriginHere.toSeq)
//    println(v3.edgesWithOriginHere.toSeq)
//    println(v4.edgesWithOriginHere.toSeq)
//    println(v5.edgesWithOriginHere.toSeq)
//
//    println(v1._incidentEdge)
//    println(v2._incidentEdge)
//    println(v3._incidentEdge)
//    println(v4._incidentEdge)
//    println(v5._incidentEdge)


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

  test("Cut bug test2") {
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
    val chain = List(V2(0.0, 200.0), V2(0.0, -200.0), V2(200.0, 0.0), V2(-200.0, 0.0))

    PlanarDCELCutOps.cutChain[DATA, LABELS](CutChain(chain), CuttingContext[DATA, LABELS](dcel, Provider))
//    println(dcel.toLongSting)
    dcel.sanityCheck()

  }

  test("Cut poly test") {
    val dcel = new PlanarDCEL[DATA](0, x => x)

    val op1 = CutChain[DATA, LABELS](Seq(V2(0, 1000), V2(0, -1000)))
    PlanarDcelCutPipelineInterpreter.cutPipeline(dcel, Provider, op1)

    val poly = Seq(V2(-200, -200), V2(200, -200), V2(200, 200), V2(-200, 200))
    val op2 = CutPoly[DATA, LABELS](PolygonRegion(poly), Some(0), Seq(1, 2, 3, 4), Seq(5, 6, 7, 8), Seq(-1, -2, -3, -4))
    val res = PlanarDcelCutPipelineInterpreter.cutPipeline(dcel, Provider, op2)

    dcel.sanityCheck()
    dcel.planarSanityCheck()
    /*
           |
     *<------------*|
     |     |       |
     |     |       |
     *-----|------>*
          |
      */

    val v1 = dcel.getVertex(V2(-200, -200)).get
    val v2 = dcel.getVertex(V2(200, -200)).get
    val v3 = dcel.getVertex(V2(200, 200)).get
    val v4 = dcel.getVertex(V2(-200, 200)).get

    val v5 = dcel.getVertex(V2(0, -200)).get
    val v6 = dcel.getVertex(V2(0, 200)).get

    assert(res.labelToVertex(-1) == Set(v1))
    assert(res.labelToVertex(-2) == Set(v2))
    assert(res.labelToVertex(-3) == Set(v3))
    assert(res.labelToVertex(-4) == Set(v4))

    assert(res.vertexToLabel(v5) == Set())
    assert(res.vertexToLabel(v6) == Set())

    assert(res.halfEdgesProduced.size == 6 * 2 + 2 * 2) //6 own + 2 splitted
    assert(res.faceProduced.size == 2)
    assert(res.vertexProduced.size == 6)

    val faces = res.faceProduced
//    println(faces)
//    println(faces.map(f => res.faceToLabel(f)))
    assert(faces.forall(f => res.faceToLabel(f) == Set(0)))
    assert(res.labelToFace(0) == faces.toSet)

    assert(dcel.getStraightEdgePathVertex(v1, v2).size == 2)
    assert(dcel.getStraightEdgePathVertex(v2, v3).size == 1)
    assert(dcel.getStraightEdgePathVertex(v3, v4).size == 2)
    assert(dcel.getStraightEdgePathVertex(v4, v1).size == 1)

    assert(dcel.getStraightEdgePathVertex(v1, v2).flatMap(he => res.halfEdgeToLabel(he)).toSet == Set(1))
    assert(dcel.getStraightEdgePathVertex(v2, v3).flatMap(he => res.halfEdgeToLabel(he)).toSet == Set(2))
    assert(dcel.getStraightEdgePathVertex(v3, v4).flatMap(he => res.halfEdgeToLabel(he)).toSet == Set(3))
    assert(dcel.getStraightEdgePathVertex(v4, v1).flatMap(he => res.halfEdgeToLabel(he)).toSet == Set(4))

    assert(dcel.getStraightEdgePathVertex(v1, v2).flatMap(he => res.halfEdgeToLabel(he.twin)).toSet == Set(5))
    assert(dcel.getStraightEdgePathVertex(v2, v3).flatMap(he => res.halfEdgeToLabel(he.twin)).toSet == Set(6))
    assert(dcel.getStraightEdgePathVertex(v3, v4).flatMap(he => res.halfEdgeToLabel(he.twin)).toSet == Set(7))
    assert(dcel.getStraightEdgePathVertex(v4, v1).flatMap(he => res.halfEdgeToLabel(he.twin)).toSet == Set(8))
  }

  test("Trace segment test") {
    val dcel = new PlanarDCEL[DATA](0, x => x)
    val op1 = CutPoly[DATA, LABELS](AARectangle(V2(-200, -200), V2(200, 200)).toPolygon)

    val op2 = TraceSegmentAtAngle[DATA, LABELS](ctx => ctx.dcel.getEdge(V2(-200, -200), V2(200, -200)).toSeq,
      0.5, 1000, utils.math.QUARTER_PI,
      lFaceLabel = Some(1), rFaceLabel = Some(2),
      lEdgeLabel = Some(3), rEdgeLabel = Some(4),
      fHalfLabel = Some(5), sHalfLabel = Some(6),
      atLabel = Some(7), rayEndOrHitPointLabel = Some(8)
    )

    PlanarDcelCutPipelineInterpreter.cutPipeline(dcel, Provider, op1)
    val res = PlanarDcelCutPipelineInterpreter.cutPipeline(dcel, Provider, op2)

    dcel.sanityCheck()
    dcel.planarSanityCheck()

    assert(res.vertexProduced.size == 2)
    assert(res.halfEdgesProduced.size == 6)
    assert(res.faceProduced.size == 1)
    val rayStart = V2(0, -200)
    val rayHit = V2(200, 0)
    val vStart = dcel.getVertex(rayStart).get
    val vHit = dcel.getVertex(rayHit).get
    assert(res.vertexToLabel(vStart) == Set(7))
    assert(res.vertexToLabel(vHit) == Set(8))

    val e = dcel.getEdge(rayStart, rayHit).get

    assert(res.halfEdgeToLabel(e) == Set(3))
    assert(res.halfEdgeToLabel(e.twin) == Set(4))
    assert(res.halfEdgeToLabel(e.prev) == Set(5))
    assert(res.halfEdgeToLabel(e.twin.next) == Set(6))

    assert(res.faceToLabel(e.leftFace) == Set(1))
    assert(res.faceToLabel(e.twin.leftFace) == Set(2))

  }

  test("Merge faces") {
    val op1 = StepsSeq[DATA, LABELS](
      (for (i <- 0 until 3; j <- 0 until 3)
        yield CutPoly[DATA, LABELS](AARectangle(V2(i, j) * 100, V2(i + 1, j + 1) * 100).toPolygon, Some(i * 3 + j))): _*)
    val op2 = StepsSeq(op1,
      MergeFaces[DATA, LABELS](ctx => ctx.labelToFace(4).headOption,
        ctx => (ctx.labelToFace(1) | ctx.labelToFace(3) | ctx.labelToFace(5) | ctx.labelToFace(7)).toSeq, Some(-1)))

    val dcel = new PlanarDCEL[DATA](0, x => x)

    val res = PlanarDcelCutPipelineInterpreter.cutPipeline(dcel, Provider, op2)
    dcel.sanityCheck()
    dcel.planarSanityCheck()
    assert(res.faceProduced.size == 9 - 4)
    assert(res.labelToFace(-1).size == 1)
    assert(dcel.asPolygon(res.labelToFace(-1).head).area ~= 100 * 100 * 5)

  }

  test("Connect vertices") {
    //todo
  }
}
