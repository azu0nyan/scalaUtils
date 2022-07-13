package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL.{DCELData, Face, HalfEdge, Vertex}
import utils.datastructures.dcel.PlanarDCELCutPipeline.{CuttingContext, Labels, PlanarDCELCutPipeline, TransformContext}

trait PlanarDCELCutPipelineMarkers {
  def MarkEdges[D <: DCELData, L <: Labels](edgeMarker: CuttingContext[D, L] => Seq[(L#HalfEdgeLabel, HalfEdge[D])]): PlanarDCELCutPipeline[D, L] =
    TransformContext[D, L] { context =>
      val selectedEdges = edgeMarker(context)
      context.addEdgeLabels(selectedEdges)
    }

  def MarkAreas[D <: DCELData, L <: Labels](areaMarker: CuttingContext[D, L] => Seq[(L#FaceLabel, Face[D])]): PlanarDCELCutPipeline[D, L] =
    TransformContext { context =>
      val selectedAreas = areaMarker(context)
      context.addFaceLabels(selectedAreas)
    }

  def MarkVertices[D <: DCELData, L <: Labels](vertexMarker: CuttingContext[D, L] => Seq[(L#VertexLabel, Vertex[D])]): PlanarDCELCutPipeline[D, L] =
    TransformContext { context =>
      val selectedVertices = vertexMarker(context)
      context.addVertexLabels(selectedVertices)
    }

  def FindAndMarkEdges[D <: DCELData, L <: Labels](fillter: HalfEdge[D] => Boolean, label: L#HalfEdgeLabel): PlanarDCELCutPipeline[D, L] =
    MarkEdges(ctx => ctx.edgesProduced.find(fillter).map(e => (label, e)).toSeq)
}
