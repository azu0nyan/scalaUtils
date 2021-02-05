package datastructures

import org.scalatest.funsuite.AnyFunSuite
import utils.datastructures.dcel.PlanarDCEL
import utils.math.planar.V2
import org.scalatest.AppendedClues._
class DcelTest extends AnyFunSuite {
  test("Triangle") {
    val dcel = new PlanarDCEL[V2, Int, Int](0, x => x)

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
    assert(e1.data == 1)
    assert(e1.twin.data == 4)
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
}
