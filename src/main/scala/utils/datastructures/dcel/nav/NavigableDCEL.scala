package utils.datastructures.dcel.nav

import utils.datastructures.dcel.{DCEL, HierarchicalDCEL}
import utils.datastructures.dcel.HierarchicalDCEL.{HierarchicalDCEL, HierarchicalDCELData, HierarchicalDCELOwnData, HierarchicalDCElDataProvider, HierarchicalEdge, HierarchicalFace, OwnDataProvider, RHalfEdge, RVertex}
import utils.datastructures.dcel.nav.DCELPath.BorderNode
//import utils.datastructures.dcel.nav.DCELPath.{BorderNode, FreeBorderNode, PortalNode}
import utils.datastructures.dcel.nav.NavigableDCEL.NavigableDCELOwnData
import utils.datastructures.dcel.nav.Portal.Portal
import utils.math.planar.V2
import utils.math._

object NavigableDCEL {

  /** Navigable DCEL is Hierarchical dcel, where own data implements NavigableFace and NavigableHalfEdge traits */
  type NavigableDCELOwnData = HierarchicalDCELOwnData {
    type HalfEdgeOwnData <: NavigableHalfEdge
    type FaceOwnData <: NavigableFace
  }

  /** Takes constructors for own data, returns data provider, required to guarantee call of setHalfEdge and setFace */
  def NavigableDCELDataProvider[D <: NavigableDCELOwnData](
                                                            vertexDataConstructor: V2 => D#VertexOwnData,
                                                            newHalfEdgeDataConstructor: (RVertex[D], RVertex[D]) => (D#HalfEdgeOwnData, D#HalfEdgeOwnData),
                                                            splitHalfEdgeDataConstructor: (RHalfEdge[D], V2) => (D#HalfEdgeOwnData, D#HalfEdgeOwnData),
                                                            faceDataConstructor: RHalfEdge[D] => D#FaceOwnData,
                                                          )(
                                                            implicit positionExtractor: D#VertexOwnData => V2
                                                          ): HierarchicalDCElDataProvider[D] = {
    //Provider for hierarchical dcel own data
    val dataProv = new OwnDataProvider[D] {
      override def newFaceData(edge: RHalfEdge[D]): D#FaceOwnData = faceDataConstructor(edge)
      override def newVertexData(v: V2): D#VertexOwnData = vertexDataConstructor(v)
      override def newEdgeData(v1: RVertex[D], v2: RVertex[D]): (D#HalfEdgeOwnData, D#HalfEdgeOwnData) = newHalfEdgeDataConstructor(v1, v2)
      override def splitEdgeData(edge: RHalfEdge[D], at: V2): (D#HalfEdgeOwnData, D#HalfEdgeOwnData) = splitHalfEdgeDataConstructor(edge, at)
    }

    new HierarchicalDCElDataProvider[D](dataProv,
      setupHalfEdge = x => x.ownData.setHalfEdge(x),
      setupFace = x => x.ownData.setFace(x),
    )(positionExtractor)
  }

  type NavigableDCEL[D <: NavigableDCELOwnData] = HierarchicalDCEL[D]



}
