package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL.{Face, HalfEdge, Vertex}
import utils.datastructures.dcel.PlanarDCELCutPipeline.{CuttingContext, Labels, PlanarDCELCutPipeline, TransformContext}

trait PlanarDCELCutPipelineMarkers {
  def MarkEdges[VD, HD, FD, VL, HL, FL](edgeMarker: CuttingContext[VD, HD, FD, VL, HL, FL] => Seq[(HL, HalfEdge[VD, HD, FD])]): PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL] =
    TransformContext[VD, HD, FD, VL, HL, FL] { context =>
      val selectedEdges = edgeMarker(context)
      context.addEdgeLabels(selectedEdges)
    }

  def MarkAreas[VD, HD, FD, VL, HL, FL](areaMarker: CuttingContext[VD, HD, FD, VL, HL, FL] => Seq[(FL, Face[VD, HD, FD])]): PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL] =
    TransformContext { context =>
      val selectedAreas = areaMarker(context)
      context.addFaceLabels(selectedAreas)
    }

  def MarkVertices[VD, HD, FD, VL, HL, FL](vertexMarker: CuttingContext[VD, HD, FD, VL, HL, FL] => Seq[(VL, Vertex[VD, HD, FD])]): PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL] =
    TransformContext { context =>
      val selectedVertices = vertexMarker(context)
      context.addVertexLabels(selectedVertices)
    }

  def FindAndMarkEdges[VD, HD, FD, VL, HL, FL](fillter: HalfEdge[VD, HD, FD] => Boolean, label: HL): PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL] =
    MarkEdges(ctx => ctx.halfEdgesProduced.find(fillter).map(e => (label, e)).toSeq)


  def ForEachHalfEdgeWithLabels[VD, HD, FD, VL, HL, FL](
                                                         func: (HalfEdge[VD, HD, FD], Set[HL]) => Unit,
                                                       ): PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL] =
    TransformContext { context =>
      context.halfEdgesProduced.foreach(he => func(he, context.halfEdgeToLabel(he)))
      context
    }

  def ForEachFaceWithLabels[VD, HD, FD, VL, HL, FL](
                                                         func: (Face[VD, HD, FD], Set[FL]) => Unit,
                                                       ): PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL] =
    TransformContext { context =>
      context.faceProduced.foreach(face => func(face, context.faceToLabel(face)))
      context
    }

  def ForEachVertexWithLabels[VD, HD, FD, VL, HL, FL](
                                                           func: (Vertex[VD, HD, FD], Set[VL]) => Unit,
                                                         ): PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL] =
    TransformContext { context =>
      context.vertexProduced.foreach(vertex => func(vertex, context.vertexToLabel(vertex)))
      context
    }
}
