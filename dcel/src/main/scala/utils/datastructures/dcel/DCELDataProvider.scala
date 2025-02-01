package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL.{DCELData, Face, HalfEdge, Vertex}
import utils.math.planar.V2


trait NewFaceDataProvider[VD, HD, FD] {
  def newFaceData(edge: HalfEdge[VD, HD, FD]): FD
}

trait NewVertexDataProvider[VD, HD, FD] {
  def newVertexData(v: V2): VD
}

trait NewEdgeDataProvider[VD, HD, FD] {
  def newEdgeData(v1: Vertex[VD, HD, FD], v2: Vertex[VD, HD, FD]): (HD, HD)
}

trait SplitEdgeDataProvider[VD, HD, FD] {
  def splitEdgeData(edge: HalfEdge[VD, HD, FD], data: V2): (HD, HD)
}

trait MergeFaceDatas[VD, HD, FD] {
  def mergeFaceDatas(main: Face[VD, HD, FD], toMerge: Face[VD, HD, FD]): FD = main.data
}

trait DCELDataProvider[VD, HD, FD]
  extends NewFaceDataProvider[VD, HD, FD]
    with NewVertexDataProvider[VD, HD, FD]
    with NewEdgeDataProvider[VD, HD, FD]
    with SplitEdgeDataProvider[VD, HD, FD]
    with MergeFaceDatas[VD, HD, FD]