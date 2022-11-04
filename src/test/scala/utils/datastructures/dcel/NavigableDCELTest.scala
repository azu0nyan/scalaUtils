package utils.datastructures.dcel

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.AppendedClues._
import utils.datastructures.dcel.HierarchicalDCEL.{HierarchicalDCELOwnData, HierarchicalFace}
import utils.datastructures.dcel.nav.DCELPath._
import utils.datastructures.dcel.nav.NavigableDCEL
import utils.datastructures.dcel.nav.NavigableDCEL.{NavigableDCELOwnData, NavigableFace, NavigableHalfEdge}
import utils.datastructures.spatial.AARectangle
import utils.math.planar.V2

class NavigableDCELTest extends AnyFunSuite {




  test("init") {

    type NavData = NavigableDCELOwnData {
      type FaceOwnData = NavigableFace
      type HalfEdgeOwnData = NavigableHalfEdge
      type VertexOwnData = V2
    }
//    val x = new HierarchicalFace[NavData](None, new NavigableFace {}, NavDataProvider[NavData]())(x => x)
  }

  test("init with custom data") {
    trait OtherFaceData{
      val fd: String
    }

    trait OtherHalfEdgeData {
      val ed: String
    }

    case class FaceData(override val fd: String) extends NavigableFace with OtherFaceData
    case class HaldEdgeData(override val ed: String) extends NavigableHalfEdge with OtherHalfEdgeData{
      override def pathNodes: Seq[BorderNode] = Seq(FreeBorderNode(this))
}

    type NavData = NavigableDCELOwnData {
      type FaceOwnData = FaceData
      type HalfEdgeOwnData = HaldEdgeData
      type VertexOwnData = V2
    }

    val prov = NavigableDCEL.NavigableDCELDataProvider[NavData](
      x => x,
      (x, y) => (new HaldEdgeData("e"), new HaldEdgeData("t")),
      (x, y) => (new HaldEdgeData("e"), new HaldEdgeData("t")),
      x => new FaceData("nf")
    )(x => x)

    val rootFace = new HierarchicalFace[NavData](None, FaceData("f"), prov)(x => x)

    assert(rootFace.ownData.fd == "f")
    assert(rootFace.ownData.hierarchicalFace == rootFace)

    val toCut = AARectangle(V2(-100, -100), V2(100, 100)).toPolygon
    val cutResult = rootFace.cutClamped(toCut)

    assert(rootFace.allChildFaces.head.ownData.fd == "nf")
    assert(rootFace.allChildFaces.head.ownData.hierarchicalFace == rootFace.allChildFaces.head)
    assert(cutResult.forall(_.data.ownData.ed == "e"))
    assert(cutResult.forall(_.twin.data.ownData.ed == "t"))

  }
}
