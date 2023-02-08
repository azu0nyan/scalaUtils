package utils.datastructures.dcel.nav


import utils.datastructures.dcel.PlanarDCELCutPipeline._
import utils.datastructures.dcel.DCEL.{DCELData, Face, HalfEdge}
import utils.datastructures.dcel.{DCEL, DCELDataProvider, PlanarDCEL, PlanarDcelCutPipelineInterpreter}
import utils.datastructures.dcel.PlanarDCELCutPipeline.{CutPoly, Labels, StepsSeq}
import utils.datastructures.dcel.nav.DCELPath.{BorderNode, NavMeshEdge, PathNode}
import utils.datastructures.dcel.nav.NavMesh.NavMeshFaceData
import utils.datastructures.graph.{ArrayBufferGraph, GraphOps}
import utils.math.Scalar
import utils.math.planar.algo.PolygonToConvex
import utils.math.planar.{Polygon, V2}


object NavMesh {


  class NavMeshHalfEdgeData(var boundTo: Option[NavigableHalfEdge] = None)
  class NavMeshFaceData(var isHole: Boolean)

  type NavMeshDcelData = DCELData {
    type VertexData = V2
    type HalfEdgeData = NavMeshHalfEdgeData
    type FaceData = NavMeshFaceData
  }

//  type NavMesh = PlanarDCEL[NavMeshDcelData]
  type NavMeshGraph =  ArrayBufferGraph[PathNode, Scalar]

  class NavMesh extends PlanarDCEL[NavMeshDcelData](new NavMeshFaceData(true), x => x) {
    def boundMeshEdge(bn: BorderNode): Option[HalfEdge[NavMeshDcelData]] = halfEdges.find(_.data.boundTo.contains(bn.border))
    def boundMeshFace(bn: BorderNode): Option[Face[NavMeshDcelData]] = halfEdges.find(_.data.boundTo.contains(bn.border)).map(_.leftFace)

  }




  def buildNavMeshFromNavigableFace(face: NavigableFace): NavMesh = {
    val res = new NavMesh
    //todo more efficient
    type CutLabels = Labels {
      type VertexLabel = Unit
      type HalfEdgeLabel = Unit
      type FaceLabel = String
    }

    val provider = new DCELDataProvider[NavMeshDcelData] {
      override def newVertexData(v: V2): V2 = v
      override def newEdgeData(v1: DCEL.Vertex[NavMeshDcelData], v2: DCEL.Vertex[NavMeshDcelData]): (NavMeshHalfEdgeData, NavMeshHalfEdgeData) = (new NavMeshHalfEdgeData(), new NavMeshHalfEdgeData)
      override def newFaceData(edge: DCEL.HalfEdge[NavMeshDcelData]): NavMeshFaceData = new NavMeshFaceData(false)
      override def splitEdgeData(edge: DCEL.HalfEdge[NavMeshDcelData], data: V2): (NavMeshHalfEdgeData, NavMeshHalfEdgeData) = (new NavMeshHalfEdgeData(), new NavMeshHalfEdgeData)
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

    val markFaces = ForEachFace[NavMeshDcelData, CutLabels](SelectFacesByLabel("HOLE"), f => f.data.isHole = true)

    val toConvex = ForEachFace[NavMeshDcelData, CutLabels](
      SelectFacesByLabel("NO_HOLE"),
      f => PolygonToConvex.toConvexPolygonsDCEL(res, f, provider)
    )

    val cutPipeline = StepsSeq(
      cutPoly,
      markEdges,
      markFaces,
      toConvex
    )

    val resCtx = PlanarDcelCutPipelineInterpreter.cutPipeline(res, provider, cutPipeline)

    res
  }

  def buildGraphFromDcel(from: PlanarDCEL[NavMeshDcelData]): NavMeshGraph = {
    val res = new NavMeshGraph

    for(e <- from.halfEdges) {
      e.data.boundTo match {
        case Some(realEdge) =>
          for(n <- realEdge.borderNodes) res.addNode(n)
        case None =>  res.addNode(NavMeshEdge(e))
      }
    }

    for(e <- from.halfEdges if e.data.boundTo.isEmpty && e.twin.data.boundTo.isEmpty) {
      res.addOrUpdateEdge(NavMeshEdge(e), NavMeshEdge(e), 0d) //opposite direction will be added since we cycle trough all edges
    }

    for(f <- from.innerFaces if !f.data.isHole;
        Seq(e1, e2) <- f.edges.toSeq.combinations(2)
        ) {
      val node1 = NavMeshEdge(e1)
      val node2 = NavMeshEdge(e2)
      val l = node1.point.distance(node2.point)
      res.addOrUpdateEdge(node1, node2, l)
      res.addOrUpdateEdge(node2, node1, l)
    }

    res
  }

  def buildNavMeshAndGraph(face: NavigableFace): (NavMesh, NavMeshGraph) = {
    val navMesh = buildNavMeshFromNavigableFace(face)
    val graph = buildGraphFromDcel(navMesh)
    (navMesh, graph)
  }


}
