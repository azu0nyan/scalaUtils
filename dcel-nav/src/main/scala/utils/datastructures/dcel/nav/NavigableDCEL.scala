package utils.datastructures.dcel.nav

import utils.datastructures.dcel.HierarchicalDCEL.*
import utils.datastructures.dcel.nav.DCELPath.BorderNode
import utils.datastructures.dcel.nav.Portal.Portal
import utils.datastructures.dcel.{DCEL, HierarchicalDCEL}
import utils.math.*
import utils.math.planar.V2


/** Navigable DCEL is Hierarchical dcel, where own data implements NavigableFace and NavigableHalfEdge traits */
object NavigableDCEL {


  /** Takes constructors for own data, returns data provider, required to guarantee call of setHalfEdge and setFace */
  def NavigableDCELDataProvider[VD, HD <: NavigableHalfEdge, FD <: NavigableFace](
                                                                                   vertexDataConstructor: V2 => VD,
                                                                                   newHalfEdgeDataConstructor: (RVertex[VD, HD, FD], RVertex[VD, HD, FD]) => (HD, HD),
                                                                                   splitHalfEdgeDataConstructor: (RHalfEdge[VD, HD, FD], V2) => (HD, HD),
                                                                                   faceDataConstructor: RHalfEdge[VD, HD, FD] => FD,
                                                                                 )(
                                                                                   implicit positionExtractor: VD => V2
                                                                                 ): HierarchicalDCElDataProvider[VD, HD, FD] = {
    //Provider for hierarchical dcel own data
    val dataProv = new OwnDataProvider[VD, HD, FD] {
      override def newFaceData(edge: RHalfEdge[VD, HD, FD]): FD = faceDataConstructor(edge)
      override def newVertexData(v: V2): VD = vertexDataConstructor(v)
      override def newEdgeData(v1: RVertex[VD, HD, FD], v2: RVertex[VD, HD, FD]): (HD, HD) = newHalfEdgeDataConstructor(v1, v2)
      override def splitEdgeData(edge: RHalfEdge[VD, HD, FD], at: V2): (HD, HD) = splitHalfEdgeDataConstructor(edge, at)
    }

    new HierarchicalDCElDataProvider[VD, HD, FD](dataProv,
      setupHalfEdge = x => x.ownData.setHalfEdge(x),
      setupFace = x => x.ownData.setFace(x),
    )(positionExtractor)
  }

  type NavigableDCEL[VD, HD <: NavigableHalfEdge, FD <: NavigableFace] = HierarchicalDCEL[VD, HD, FD]


}
