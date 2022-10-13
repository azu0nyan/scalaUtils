package utils.datastructures.dcel

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.AppendedClues._
import utils.datastructures.dcel.HierarchicalDCEL.{HierarchicalDCELOwnData, HierarchicalFace}
import utils.datastructures.dcel.nav.NavigableDCEL.{NavDataProvider, NavigableDCELOwnData, NavigableFace, NavigableHalfEdge}
import utils.math.planar.V2

class NavigableDCELTest extends AnyFunSuite {




  test("init") {

    type NavData = NavigableDCELOwnData {
      type FaceOwnData = NavigableFace
      type HalfEdgeOwnData = NavigableHalfEdge
      type VertexOwnData = V2
    }
    val x = new HierarchicalFace[NavData](None, new NavigableFace {}, NavDataProvider[NavData]())(x => x)
  }

  test("init with custom data") {
    trait OtherData{
//      var
    }
  }
}
