package utils.datastructures.dcel.nav

import utils.datastructures.dcel.{DCEL, HierarchicalDCEL}
import utils.datastructures.dcel.HierarchicalDCEL.{HierarchicalDCEL, HierarchicalDCELData, HierarchicalDCELOwnData, HierarchicalDCElDataProvider, HierarchicalEdge, HierarchicalFace}
import utils.datastructures.dcel.nav.NavigableDCEL.NavigableDCELOwnData
import utils.math.planar.V2

object NavigableDCEL {

  type NavigableDCELOwnData  = HierarchicalDCELOwnData {
    type HalfEdgeOwnData <: NavigableHalfEdge
    type FaceOwnData <: NavigableFace
  }

  case class NavDataProvider[D <: NavigableDCELOwnData]() extends HierarchicalDCElDataProvider[D]{
    override def newFaceData(edge: DCEL.HalfEdge[HierarchicalDCELData[D]]): HierarchicalFace[D] = ???
    override def newVertexData(v: V2): HierarchicalDCEL.HierarchicalVertex[D] = ???
    override def newEdgeData(v1: DCEL.Vertex[HierarchicalDCELData[D]], v2: DCEL.Vertex[HierarchicalDCELData[D]]): (HierarchicalEdge[D], HierarchicalEdge[D]) = ???
    override def splitEdgeData(edge: DCEL.HalfEdge[HierarchicalDCELData[D]], data: V2): (HierarchicalEdge[D], HierarchicalEdge[D]) = ???
  }


  type NavigableDCEL[D <: NavigableDCELOwnData] = HierarchicalDCEL[D]



//  new HierarchicalFace[NavigableDCELOwnData](None, new NavigableFace {}, )


  trait NavigableHalfEdge{
    def setHalfEdge(he: HierarchicalEdge[HierarchicalDCELOwnData]): Unit = ???
  }

  trait NavigableFace{
    def setFace(face: HierarchicalFace[HierarchicalDCELOwnData]): Unit = ???
  }

}
