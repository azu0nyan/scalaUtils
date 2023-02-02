package utils.datastructures.dcel.nav


import utils.datastructures.dcel.PlanarDCELCutPipeline._
import utils.datastructures.dcel.DCEL.DCELData
import utils.datastructures.dcel.{DCEL, DCELDataProvider, PlanarDCEL, PlanarDcelCutPipelineInterpreter}
import utils.datastructures.dcel.PlanarDCELCutPipeline.{CutPoly, Labels, StepsSeq}
import utils.math.planar.algo.PolygonToConvex
import utils.math.planar.{Polygon, V2}


object NavMesh {

  class NavMeshHalfEdge(var boundTo: Option[NavigableHalfEdge] = None)
  class NavMeshFace()

  type NavMeshDcelData = DCELData {
    type VertexData = V2
    type HalfEdgeData = NavMeshHalfEdge
    type FaceData = NavMeshFace
  }

  def buildFromNavigableFace(face: NavigableFace): PlanarDCEL[NavMeshDcelData] = {
    val res = new PlanarDCEL[NavMeshDcelData](new NavMeshFace(), x => x)
    //todo more efficient
    type CutLabels = Labels {
      type VertexLabel = Unit
      type HalfEdgeLabel = Unit
      type FaceLabel = String
    }

    val outer = face.hierarchicalFace.ownArea
    val nested = Polygon.toNestedPolygons(outer.regions)
    val partitioned = Polygon.partitionNested(nested)

    val cutPoly = StepsSeq[NavMeshDcelData, CutLabels](
      partitioned.zipWithIndex.map { case (poly, i) =>
        StepsSeq[NavMeshDcelData, CutLabels](
          poly.map(p => CutPoly[NavMeshDcelData, CutLabels](p, Some(if (i % 2 == 0) "NO_HOLE" else "HOLE"))): _*
        )
      }: _*)

    val edgesToCheck = face.hierarchicalFace.fullBorder ++ face.hierarchicalFace.innerDCEL.outerFace.holesContours.flatten.toSeq
    val markEdges = StepsSeq[NavMeshDcelData, CutLabels](
      (for (edge <- edgesToCheck) yield ForEachEdge[NavMeshDcelData, CutLabels](
        SelectEdgesEqualToSegmentAndSameDirection(face.hierarchicalFace.innerDCEL.asSegment(edge)),
        e => e.data.boundTo = Some(edge.data.ownData))): _*
    )

    val cutPipeline = StepsSeq(
      cutPoly,
      markEdges
    )

    val provider = new DCELDataProvider[NavMeshDcelData] {
      override def newVertexData(v: V2): V2 = v
      override def newEdgeData(v1: DCEL.Vertex[NavMeshDcelData], v2: DCEL.Vertex[NavMeshDcelData]): (NavMeshHalfEdge, NavMeshHalfEdge) = (new NavMeshHalfEdge(), new NavMeshHalfEdge)
      override def newFaceData(edge: DCEL.HalfEdge[NavMeshDcelData]): NavMeshFace = new NavMeshFace()
      override def splitEdgeData(edge: DCEL.HalfEdge[NavMeshDcelData], data: V2): (NavMeshHalfEdge, NavMeshHalfEdge) = (new NavMeshHalfEdge(), new NavMeshHalfEdge)
    }

    val resCtx = PlanarDcelCutPipelineInterpreter.cutPipeline(res, provider, cutPipeline)

    PolygonToConvex.toConvexPolygonsDCEL()

    res
  }


}
