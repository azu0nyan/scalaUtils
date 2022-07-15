package utils.datastructures.dcel

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.AppendedClues._
import utils.datastructures.dcel.DCELOps
import utils.datastructures.dcel.HierarchicalDCEL.{HierarchicalDCELOwnData, HierarchicalFace, OwnDataProvider, RHalfEdge, RVertex}
import utils.datastructures.spatial.AARectangle
import utils.math.planar.{PolygonRegion, SegmentPlanar, V2}

import java.util.concurrent.atomic.AtomicInteger

class HierarchicalDCELTest extends AnyFunSuite {

  val x = new AtomicInteger()
  type HData = HierarchicalDCELOwnData {
    type VertexOwnData = V2
    type HalfEdgeOwnData = String
    type FaceOwnData = String
  }
  class DataProvider() extends OwnDataProvider[HData] {
    override def newFaceData(edge: RHalfEdge[HData]): String = x.getAndIncrement().toString

    override def newVertexData(v: V2): V2 = v

    override def newEdgeData(v1: RVertex[HData], v2: RVertex[HData]): (String, String) = (x.getAndIncrement().toString, x.getAndIncrement().toString)

    override def splitEdgeData(edge: RHalfEdge[HData], at: V2): (String, String) = (x.getAndIncrement().toString, x.getAndIncrement().toString)
  }


  def cutInside(face: HierarchicalFace[HData], toCut: PolygonRegion) = {
    face.cutClamped(toCut, new DataProvider() )
  }

  test("Single rectangle cut") {

    val root = new HierarchicalFace[HData](None, "ROOT")(x => x)
    val toCut = AARectangle(V2(-100, -100), V2(100, 100)).toPolygon
    val cutResult = cutInside(root, toCut)

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


    val childCutResult = cutInside(child, toCut)

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
      assert(e.data.parents.head.face == child.face)
      assert(e.twin.data.parents.isEmpty)

      assert(e.leftFace == cildsChids.face)
      assert(e.twin.leftFace == child.innerDCEL.outerFace)
    }
  }


  /*
    |_______________________
    |    ______________    |
    |   |              |   |
    |   |         _    |   |
    |   |  _     |_|   |   |
    |   |_|_|__________|   |
    |                      |
    |______________________

   */
  test("Parent correctness") {
    val root = new HierarchicalFace[HData](None, "ROOT")(x => x)
    val containerShape = AARectangle(V2(0, 0), V2(100, 100)).toPolygon
    val containerHole1 = AARectangle(V2(20, 20), V2(30, 30)).toPolygon
    val containerHole2 = AARectangle(V2(40, 30), V2(50, 40)).toPolygon
    val holeContainerIntersector = AARectangle(V2(10, 30), V2(60, 60)).toPolygon

    val cutContainerResult = cutInside(root, containerShape)
    val container = DCELOps.selectToTheLeft(cutContainerResult).toSeq.head
    val cutHoleResult1 = cutInside(root, containerHole1)
    val hole1 = DCELOps.selectToTheLeft(cutHoleResult1).toSeq.head
    val cutHoleResult2 = cutInside(root, containerHole2)
    val hole2 = DCELOps.selectToTheLeft(cutHoleResult2).toSeq.head

    assert(root.directChildFaces.toSet == Set(container.data, hole1.data, hole2.data))

    assert(container.holes.size == 2)
    assert(container.holes.toSet == Set(hole1, hole2))


    val segmentWithParens = SegmentPlanar(V2(10.0, 30.0), V2(60.0, 30.0))
    val potentialParents = container.data.findParentForEdge(segmentWithParens)
    println(s"Potential parents for $segmentWithParens is ${potentialParents.map(_.asSegment)}")
    assert(potentialParents.size == 1) //other parented segment will be twin

    val inContainerCutResult = cutInside(container.data, holeContainerIntersector)
    val inContainerCutFace = DCELOps.selectToTheLeft(inContainerCutResult).toSeq.head

    println(inContainerCutFace.edges.toSeq.map(_.data.asSegment))
    val edgesWithParent = inContainerCutFace.edges.filter(_.data.parents.nonEmpty).toSeq
    println(edgesWithParent.map(_.data.asSegment))
    println(edgesWithParent.map(_.data.parents.map(_.asSegment)))
    assert(edgesWithParent.size == 1)
    assert(edgesWithParent.head.data.asSegment == SegmentPlanar(V2(10, 30), V2(60, 30)))
    assert(edgesWithParent.head.data.parents.size == 1)
    assert(edgesWithParent.head.data.parents.head.asSegment == SegmentPlanar(V2(20, 30), V2(30, 30)))
    assert(edgesWithParent.head.twin.data.parents.size == 1)
    assert(edgesWithParent.head.twin.data.parents.head.asSegment == SegmentPlanar(V2(50, 30), V2(40, 30)))


  }
}
