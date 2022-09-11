package utils.datastructures.dcel

import utils.datastructures.dcel.HierarchicalDCEL.HierarchicalDCELOwnData

object NavigableDCEL {

  type NavigableDCELOwnData  = HierarchicalDCELOwnData {
    type HalfEdgeOwnData <: NavigableHalfEdge
    type FaceOwnData <: NavigableFace
  }

  trait NavigableHalfEdge

  trait NavigableFace


}
