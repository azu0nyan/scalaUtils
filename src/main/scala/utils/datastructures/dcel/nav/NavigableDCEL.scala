package utils.datastructures.dcel.nav

import utils.datastructures.dcel.HierarchicalDCEL.{HierarchicalDCELOwnData, HierarchicalEdge}

object NavigableDCEL {

  type NavigableDCELOwnData  = HierarchicalDCELOwnData {
    type HalfEdgeOwnData <: NavigableHalfEdge
    type FaceOwnData <: NavigableFace
  }

  trait NavigableHalfEdge{
    def setHalfEdge(he: HierarchicalEdge[NavigableDCELOwnData])
  }

  trait NavigableFace


}
