package utils.datastructures.dcel.nav

import utils.datastructures.dcel.{DCEL, HierarchicalDCEL}
import utils.datastructures.dcel.HierarchicalDCEL.{HierarchicalDCEL, HierarchicalDCELData, HierarchicalDCELOwnData, HierarchicalDCElDataProvider, HierarchicalEdge, HierarchicalFace}
import utils.math.planar.V2

object NavigableDCEL {

  type NavigableDCELOwnData  = HierarchicalDCELOwnData {
    type NavigableHalfEdge <: NavigableHalfEdge
    type FaceOwnData <: NavigableFace
    type VertexData = V2
  }

  object DataProvider extends HierarchicalDCElDataProvider[NavigableDCELOwnData]{
    override def newEdgeData(v1: DCEL.Vertex[HierarchicalDCELData[NavigableDCELOwnData]], v2: DCEL.Vertex[HierarchicalDCELData[NavigableDCELOwnData]]): (HierarchicalEdge[NavigableDCELOwnData], HierarchicalEdge[NavigableDCELOwnData]) = ???
    override def newVertexData(v: V2): HierarchicalDCEL.HierarchicalVertex[NavigableDCELOwnData] = ???
    override def newFaceData(edge: DCEL.HalfEdge[HierarchicalDCELData[NavigableDCELOwnData]]): HierarchicalFace[NavigableDCELOwnData] = ???
    override def splitEdgeData(edge: DCEL.HalfEdge[HierarchicalDCELData[NavigableDCELOwnData]], data: V2): (HierarchicalEdge[NavigableDCELOwnData], HierarchicalEdge[NavigableDCELOwnData]) = ???
  }

  type NavigableDCEL = HierarchicalDCEL[NavigableDCELOwnData]

  new HierarchicalFace[NavigableDCELOwnData](None, new NavigableFace {}, )


  trait NavigableHalfEdge{
    def setHalfEdge(he: HierarchicalEdge[NavigableDCELOwnData])
  }

  trait NavigableFace


}
