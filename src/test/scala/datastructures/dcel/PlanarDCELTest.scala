package datastructures.dcel

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.AppendedClues._
import utils.datastructures.dcel.DCEL.RawFace
import utils.datastructures.dcel.{DCELOps, PlanarDCEL}
import utils.datastructures.spatial.AARectangle
import utils.math.planar.V2

class PlanarDCELTest extends AnyFunSuite {
  type DCEL = PlanarDCEL[V2, Unit, Unit]
  type Face = RawFace[V2, Unit, Unit]

  def cut(dcel: DCEL, poly: Seq[V2]): Face = {
    val res = dcel.cutPoly(poly, x => x, (a, b) => ((), ()), (a, b) => ((), ()), x => ())
    res.head.edgeTo(res(1)).get.leftFace
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
