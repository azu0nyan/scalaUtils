package datastructures.dcel

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.AppendedClues._
import utils.datastructures.dcel.DCELOps
import utils.datastructures.dcel.HierarchicalDCEL.HierarchicalFace
import utils.datastructures.spatial.AARectangle
import utils.math.planar.V2

import java.util.concurrent.atomic.AtomicInteger

class HierarchicalDCEL extends AnyFunSuite {
  test("Single rectangle cut") {
    val x = new AtomicInteger()
    val root = new HierarchicalFace[V2, String, String](None, "ROOT")(x => x)
    val toCut = AARectangle(V2(-100, -100), V2(100, 100)).toPolygon
    val cutResult = root.cutClamped(toCut, x => x, (a, b) => (x.getAndIncrement().toString, x.getAndIncrement().toString),
      (a, b) => (x.getAndIncrement().toString, x.getAndIncrement().toString), a => x.getAndIncrement().toString)

    assert(cutResult.size == 4)
    assert(root.directChildFaces.size == 1)
    assert(root.allChildFaces.size == 1)


    val polySeq = DCELOps.selectToTheLeft(cutResult).toSeq
    assert(polySeq.size == 1)
    val child = polySeq.head.data
    assert(child.parent.contains(root))

    for (e <- cutResult) {
      assert(e.leftFace == child.face)
      assert(e.twin.leftFace == root.innerDCEL.outerFace)
    }


    val childCutResult = child.cutClamped(toCut, x => x, (a, b) => (x.getAndIncrement().toString, x.getAndIncrement().toString),
      (a, b) => (x.getAndIncrement().toString, x.getAndIncrement().toString), a => x.getAndIncrement().toString)

    val childPolySeq = DCELOps.selectToTheLeft(childCutResult).toSeq
    assert(polySeq.size == 1)
    val cildsChids = childPolySeq.head.data
    assert(cildsChids.parent.contains(child))


    assert(childCutResult.size == 4)
    assert(child.directChildFaces.size == 1)
    assert(child.allChildFaces.size == 1)
    assert(root.allChildFaces.size == 2)
    assert(root.directChildFaces.size == 1)
    for (e <- childCutResult) {
      assert(e.data.parents.size == 1)
      assert(e.twin.data.parents.isEmpty)

      assert(e.leftFace == cildsChids.face)
      assert(e.twin.leftFace == child.innerDCEL.outerFace)

    }


  }
}
