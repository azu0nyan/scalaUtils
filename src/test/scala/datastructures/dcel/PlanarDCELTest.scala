package datastructures.dcel

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.AppendedClues._
import utils.datastructures.dcel.DCEL.{DCELData, Face}
import utils.datastructures.dcel.{DCEL, DCELDataProvider, DCELOps, PlanarDCEL}
import utils.datastructures.spatial.AARectangle
import utils.math.planar.V2

class PlanarDCELTest extends AnyFunSuite {
  type Data = DCELData {
    type VertexData = V2
    type HalfEdgeData = Unit
    type FaceData = Unit
  }
  type DCEL = PlanarDCEL[Data]
  type Face = DCEL.Face[Data]

  def cut(dcel: DCEL, poly: Seq[V2]): Face = {
    val res = dcel.cutPoly(poly, new DCELDataProvider[Data] {
      override def newFaceData(edge: DCEL.HalfEdge[Data]): Unit = ()
      override def newVertexData(v: _root_.utils.math.planar.V2): PlanarDCELTest.this.Data#VertexData = v
      override def newEdgeData(v1: _root_.utils.datastructures.dcel.DCEL.Vertex[PlanarDCELTest.this.Data], v2: _root_.utils.datastructures.dcel.DCEL.Vertex[PlanarDCELTest.this.Data]): (PlanarDCELTest.this.Data#HalfEdgeData, PlanarDCELTest.this.Data#HalfEdgeData) = ((), ())
      override def splitEdgeData(edge: _root_.utils.datastructures.dcel.DCEL.HalfEdge[PlanarDCELTest.this.Data], data: _root_.utils.math.planar.V2): (PlanarDCELTest.this.Data#HalfEdgeData, PlanarDCELTest.this.Data#HalfEdgeData) = ((), ())
    })
    res.head.edgeTo(res(1)).get.leftFace
  }

  test("Triangle") {
    val dcel = new DCEL(0, x => x)

    val f = dcel.makeFace(1)
    assert(dcel.innerFaces.contains(f))

    val v1 = dcel.makeVertex(V2(100, 100))
    assert(dcel.vertices.contains(v1))
    val v2 = dcel.makeVertex(V2(200, 500))
    assert(dcel.vertices.contains(v2))
    val v3 = dcel.makeVertex(V2(500, 500))
    assert(dcel.vertices.contains(v3))

    val e1 = dcel.makeEdge(v1, v2, f, dcel.outerFace, 1, 4)
    assert(e1.next == e1.twin) withClue s"e1.next should be twin"
    //    assert(e1.data == 1)
    //    assert(e1.twin.data == 4)
    assert(f.incidentEdge.contains(e1))
    assert(v1.edgesWithOriginHere.toSeq.equals(Seq(e1))) withClue s"${v1.edgesWithOriginHere.toSeq}"
    assert(v2.edgesWithOriginHere.toSeq.equals(Seq(e1.twin))) withClue s"${v2.edgesWithOriginHere.toSeq}"

    val e2 = dcel.makeEdge(v2, v3, f, dcel.outerFace, 2, 5)
    assert(e1.next == e2) withClue s"e1.next != e2; e1.next = ${e1.next.data}"
    assert(e2.next == e2.twin) withClue s"e2.next != e2; e2.next = ${e2.next.data}"


    val e3 = dcel.makeEdge(v3, v1, f, dcel.outerFace, 3, 6)
    assert(e1.next == e2) withClue s"e1.next == e2; e1.next = ${e1.next.data}"
    assert(e2.next == e3) withClue s"e2.next == e3; e2.next = ${e2.next.data}"
    assert(e3.next == e1) withClue s"e3.next == e1; e3.next = ${e3.next.data}"
  }


  test("vertices merge case 1") {
    val dcel = new DCEL((), x => x)
    val outer = cut(dcel, AARectangle(V2(-100, -100), V2(100, 100)).toPolygon.vertices)
    val inner = cut(dcel, AARectangle(V2(-50, -50), V2(50, 50)).toPolygon.vertices)

    assert(dcel.outerFace.holes.size == 1)
    assert(outer.holes.size == 1)
    assert(outer.borderEdges.size == 4)
    assert(outer.edges.size == 8)
    assert(inner.holes.size == 0)
    assert(inner.edges.size == 4)

    dcel.mergeVertices(dcel.getVertex(V2(-100, -100)).get, dcel.getVertex(V2(-50, -50)).get, x => ())

    assert(dcel.outerFace.holes.size == 1)
    assert(outer.holes.size == 0)
    assert(outer.borderEdges.size == 8)
    assert(outer.edges.size == 8)
    assert(inner.edges.size == 4)
  }

  test("vertices merge case 2") {
    val dcel = new DCEL((), x => x)
    val outer = cut(dcel, AARectangle(V2(-100, -100), V2(100, 100)).toPolygon.vertices)
    val inner = cut(dcel, AARectangle(V2(-50, -50), V2(50, 50)).toPolygon.vertices)

    assert(dcel.outerFace.holes.size == 1)
    assert(outer.holes.size == 1)
    assert(outer.borderEdges.size == 4)
    assert(outer.edges.size == 8)
    assert(inner.holes.size == 0)
    assert(inner.edges.size == 4)

    dcel.mergeVertices(dcel.getVertex(V2(-50, -50)).get, dcel.getVertex(V2(-100, -100)).get, x => ())

    assert(dcel.outerFace.holes.size == 1)
    assert(outer.holes.size == 0)
    assert(outer.borderEdges.size == 8)
    assert(outer.edges.size == 8)
    assert(inner.edges.size == 4)
  }

  test("vertices merge case 3") {
    val dcel = new DCEL((), x => x)
    val p1 = cut(dcel, AARectangle(V2(0, 0), V2(10, 10)).toPolygon.vertices)
    val p2 = cut(dcel, AARectangle(V2(20, 0), V2(30, 10)).toPolygon.vertices)
    assert(dcel.innerFaces.size == 2)
    assert(dcel.outerFace.holes.size == 2)
    assert(p1.borderEdges.size == 4)
    assert(p2.borderEdges.size == 4)

    dcel.mergeVertices(dcel.getVertex(V2(10, 10)).get, dcel.getVertex(V2(20, 10)).get, x => ())

    assert(dcel.outerFace.holes.size == 1)
    assert(dcel.innerFaces.size == 2)
    assert(p1.borderEdges.size == 4)
    assert(p2.borderEdges.size == 4)

  }

  test("vertices merge case 4-hole, new face would contain outer face's hole") {
    val dcel = new DCEL((), x => x)
    val p1 = cut(dcel, Seq(V2(200.0, 0.0), V2(400.0, 0.0), V2(400.0, 200.0), V2(200.0, 200.0), V2(300.0, 100.0)))
    val p2 = cut(dcel, Seq(V2(0.0, 0.0), V2(200.0, 0.0), V2(100.0, 100.0), V2(100.0, 200.0), V2(0.0, 200.0)))

    assert(dcel.innerFaces.size == 2)
    assert(dcel.outerFace.holes.size == 1)
    assert(dcel.outerFace.holesEdges.size == 10)
    assert(p1.borderEdges.size == 5)
    assert(p2.borderEdges.size == 5)

    val p3 = cut(dcel, List(V2(167.0, 78.0), V2(171.0, 59.0), V2(198.0, 57.0), V2(199.0, 83.0)))
    val p4 = cut(dcel, AARectangle(V2(-100, -100), V2(-90, -90)).toPolygon.vertices)
    assert(dcel.innerFaces.size == 4)
    assert(dcel.outerFace.holes.size == 3)
    assert(dcel.outerFace.holesEdges.size == 10 + 4 + 4)

    dcel.mergeVertices(dcel.getVertex(V2(100, 200)).get, dcel.getVertex(V2(200, 200)).get, x => ())

    val newFace = dcel.innerFaces.filter(f => f != p1 && f != p2 && f != p3 && f != p4).head
    assert(newFace.holes.toSeq == Seq(p3))

    assert(dcel.innerFaces.size == 5)
    assert(dcel.outerFace.holes.size == 2)
    assert(dcel.outerFace.holesEdges.size == 6 + 4)
  }

  test("vertices merge case 5-hole") {
    val dcel = new DCEL((), x => x)
    val p1 = cut(dcel, Seq(V2(100.0, 0.0), V2(200.0, 0.0), V2(200.0, 100.0)))
    val p2 = cut(dcel, Seq(V2(0.0, 0.0), V2(100.0, 0.0), V2(0.0, 100.0)))
    assert(dcel.innerFaces.size == 2)
    assert(dcel.outerFace.holes.size == 1)
    assert(dcel.halfEdges.size == 6 * 2)

    dcel.mergeVertices(dcel.getVertex(V2(0, 100)).get, dcel.getVertex(V2(200, 100)).get, x => ())

    assert(dcel.halfEdges.size == 5 * 2)
    assert(dcel.innerFaces.size == 2)
    assert(dcel.outerFace.holes.size == 1)
  }

  test("vertices merge case 4-no-hole") {
    val dcel = new DCEL((), x => x)
    val p1 = cut(dcel, List(V2(100.0, 0.0), V2(300.0, 0.0), V2(300.0, 400.0), V2(100.0, 400.0), V2(200.0, 300.0), V2(200.0, 200.0), V2(200.0, 100.0)))
    val p2 = cut(dcel, List(V2(0.0, 0.0), V2(100.0, 0.0), V2(100.0, 200.0), V2(100.0, 400.0), V2(0.0, 400.0)))
    val hole1 = cut(dcel, List(V2(115.0, 93.0), V2(117.0, 69.0), V2(139.0, 77.0), V2(139.0, 90.0)))
    val hole2 = cut(dcel, List(V2(117.0, 314.0), V2(118.0, 284.0), V2(132.0, 290.0), V2(128.0, 317.0)))

    assert(dcel.innerFaces.size == 5)
    val currentFaces = dcel.innerFaces.toSet
    val insideFace = (currentFaces &~ Set(p1, p2, hole1, hole2)).head
    assert(insideFace.holes.size == 2)

    dcel.mergeVertices(dcel.getVertex(V2(100, 200)).get, dcel.getVertex(V2(200, 200)).get, x => ())

    val newFace = (dcel.innerFaces.toSet &~ currentFaces).head
    assert(dcel.innerFaces.size == 6)
    assert(newFace.holes.size == 1)
    assert(insideFace.holes.size == 1)


  }

  test("vertices merge case 5-no-hole") {
    val dcel = new DCEL((), x => x)
    val p1 = cut(dcel, List(V2(200.0, 0.0), V2(400.0, 0.0), V2(400.0, 300.0), V2(200.0, 300.0), V2(300.0, 200.0), V2(300.0, 100.0)))
    val p2 = cut(dcel, List(V2(0.0, 0.0), V2(200.0, 0.0), V2(100.0, 100.0), V2(100.0, 200.0), V2(200.0, 300.0), V2(0.0, 300.0)))
    assert(dcel.innerFaces.size == 3)
    val edges = dcel.halfEdges.size

    dcel.mergeVertices(dcel.getVertex(V2(100, 100)).get, dcel.getVertex(V2(300, 100)).get, x => ())

    assert(dcel.innerFaces.size == 3)
    assert(dcel.halfEdges.size == edges - 2)


  }

  test("vertices merge case 6-no-hole") {
    val dcel = new DCEL((), x => x)
    val p1 = cut(dcel, List(V2(200.0, 0.0), V2(400.0, 0.0), V2(400.0, 200.0), V2(200.0, 200.0), V2(300.0, 100.0)))
    val p2 = cut(dcel, List(V2(0.0, 0.0), V2(200.0, 0.0), V2(100.0, 100.0), V2(200.0, 200.0), V2(0.0, 200.0)))

    assert(dcel.innerFaces.size == 3)
    assert(dcel.halfEdges.size == 20)

    dcel.mergeVertices(dcel.getVertex(V2(100, 100)).get, dcel.getVertex(V2(300, 100)).get, x => ())

    assert(dcel.innerFaces.size == 2)
    assert(dcel.halfEdges.size == 16)

    assert(p1.edges.count(_.twin.leftFace == p2) == 2)
    assert(p2.edges.count(_.twin.leftFace == p1) == 2)
  }
}
