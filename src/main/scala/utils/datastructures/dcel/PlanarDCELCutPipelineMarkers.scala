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
    MarkEdges(ctx => ctx.halfEdgesProduced.find(fillter).map(e => (label, e)).toSeq)


  def ForEachHalfEdgeWithLabels[D <: DCELData, L <: Labels](
                                                         func: (HalfEdge[D], Set[L#HalfEdgeLabel]) => Unit,
                                                       ): PlanarDCELCutPipeline[D, L] =
    TransformContext { context =>
      context.halfEdgesProduced.foreach(he => func(he, context.halfEdgeToLabel(he)))
      context
    }

  def ForEachFaceWithLabels[D <: DCELData, L <: Labels](
                                                         func: (Face[D], Set[L#FaceLabel]) => Unit,
                                                       ): PlanarDCELCutPipeline[D, L] =
    TransformContext { context =>
      context.faceProduced.foreach(face => func(face, context.faceToLabel(face)))
      context
    }

  def ForEachVertexWithLabels[D <: DCELData, L <: Labels](
                                                           func: (Vertex[D], Set[L#VertexLabel]) => Unit,
                                                         ): PlanarDCELCutPipeline[D, L] =
    TransformContext { context =>
      context.vertexProduced.foreach(vertex => func(vertex, context.vertexToLabel(vertex)))
      context
    }
}
