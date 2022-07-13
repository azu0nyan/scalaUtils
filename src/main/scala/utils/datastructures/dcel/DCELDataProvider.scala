package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL.{DCELData, HalfEdge, Vertex}
import utils.math.planar.V2


trait NewFaceDataProvider[D <: DCELData] {
  def newFaceData(edge: HalfEdge[D]): D#FaceData
}

trait NewVertexDataProvider[D <: DCELData] {
  def newVertexData(v: V2): D#VertexData
}

trait NewEdgeDataProvider[D <: DCELData] {
  def newEdgeData(v1: Vertex[D], v2: Vertex[D]): (D#HalfEdgeData, D#HalfEdgeData)
}

trait SplitEdgeDataProvider[D <: DCELData] {
  def splitEdgeData(edge: HalfEdge[D], data: V2): (D#HalfEdgeData, D#HalfEdgeData)
}

trait DCELDataProvider[D <: DCELData]
  extends NewFaceDataProvider[D]
    with NewVertexDataProvider[D]
    with NewEdgeDataProvider[D]
    with SplitEdgeDataProvider[D]