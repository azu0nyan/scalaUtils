package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL._
import utils.math.Scalar
import utils.math.planar.{PolygonRegion, V2}
import utils.math.planar.patch.Path.Path
import utils.sugar.MapOps

object PlanarDCELCutPipeline
  extends PlanarDCELCutPipelineSelectors
    with PlanarDCELCutPipelineMarkers
    with PlanarDcelCutPipelineExtensions {

  type Labels = {
    type VertexLabel
    type HalfEdgeLabel
    type FaceLabel
  }


  sealed trait PlanarDCELCutPipeline[D <: DCELData, L <: Labels] {

  }

  case class CuttingContext[D <: DCELData, L <: Labels](dcel: PlanarDCEL[D],
                                                        provider: DCELDataProvider[D],
                                                        vertexProduced: Set[Vertex[D]] = Set[Vertex[D]](),
                                                        halfEdgesProduced: Set[HalfEdge[D]] = Set[HalfEdge[D]](),
                                                        faceProduced: Set[Face[D]] = Set[Face[D]](),
                                                        labelToHalfEdge: Map[L#HalfEdgeLabel, Set[HalfEdge[D]]] = Map[L#HalfEdgeLabel, Set[HalfEdge[D]]](),
                                                        labelToFace: Map[L#FaceLabel, Set[Face[D]]] = Map[L#FaceLabel, Set[Face[D]]](),
                                                        labelToVertex: Map[L#VertexLabel, Set[Vertex[D]]] = Map[L#VertexLabel, Set[Vertex[D]]]()) {
    def addVertices(vertices: Iterable[Vertex[D]]): CuttingContext[D, L] = copy(vertexProduced = vertexProduced | vertices.toSet)
    def addFaces(areas: Iterable[Face[D]]): CuttingContext[D, L] = copy(faceProduced = faceProduced | areas.toSet)
    def addHalfEdges(edges: Iterable[HalfEdge[D]]): CuttingContext[D, L] = copy(halfEdgesProduced = halfEdgesProduced | edges.toSet)

    def removeVertices(vertices: Iterable[Vertex[D]]): CuttingContext[D, L] = copy(vertexProduced = vertexProduced &~ vertices.toSet)
    def removeFaces(areas: Iterable[Face[D]]): CuttingContext[D, L] = copy(faceProduced = faceProduced &~ areas.toSet)
    def removeHalfEdges(edges: Iterable[HalfEdge[D]]): CuttingContext[D, L] = copy(halfEdgesProduced = halfEdgesProduced &~ edges.toSet)

    def addEdgeLabels(labels: Iterable[(L#HalfEdgeLabel, HalfEdge[D])]): CuttingContext[D, L] = copy(labelToHalfEdge = MapOps.addSeqToMapToSet(labelToHalfEdge, labels.toSeq))
    def addFaceLabels(labels: Iterable[(L#FaceLabel, Face[D])]): CuttingContext[D, L] = copy(labelToFace = MapOps.addSeqToMapToSet(labelToFace, labels.toSeq))
    def addVertexLabels(labels: Iterable[(L#VertexLabel, Vertex[D])]): CuttingContext[D, L] = copy(labelToVertex = MapOps.addSeqToMapToSet(labelToVertex, labels.toSeq))
    //maybe store in reverse map to speed up??
    def vertexToLabel(v: Vertex[D]): Set[L#VertexLabel] = labelToVertex.filter(_._2.contains(v)).keySet
    def halfEdgeToLabel(h: HalfEdge[D]): Set[L#HalfEdgeLabel] = labelToHalfEdge.filter(_._2.contains(h)).keySet
    def faceToLabel(f: Face[D]): Set[L#FaceLabel] = labelToFace.filter(_._2.contains(f)).keySet

  }

  type SingleFaceSelector[D <: DCELData, L <: Labels] = CuttingContext[D, L] => Option[Face[D]]
  type FaceSelector[D <: DCELData, L <: Labels] = CuttingContext[D, L] => Seq[Face[D]]
  type SingleEdgeSelector[D <: DCELData, L <: Labels] = CuttingContext[D, L] => Option[HalfEdge[D]]
  type EdgeSelector[D <: DCELData, L <: Labels] = CuttingContext[D, L] => Seq[HalfEdge[D]]
  type SingleVertexSelector[D <: DCELData, L <: Labels] = CuttingContext[D, L] => Option[Vertex[D]]
  type VertexSelector[D <: DCELData, L <: Labels] = CuttingContext[D, L] => Seq[Vertex[D]]
  type Predicate[D <: DCELData, L <: Labels] = CuttingContext[D, L] => Boolean

  //Structural
  case class ProducePipeline[D <: DCELData, L <: Labels](producer: CuttingContext[D, L] => PlanarDCELCutPipeline[D, L]) extends PlanarDCELCutPipeline[D, L]
  case class TransformContext[D <: DCELData, L <: Labels](transformer: CuttingContext[D, L] => CuttingContext[D, L]) extends PlanarDCELCutPipeline[D, L]
  case class StepsSeq[D <: DCELData, L <: Labels](steps: PlanarDCELCutPipeline[D, L]*) extends PlanarDCELCutPipeline[D, L]
  case class If[D <: DCELData, L <: Labels](pred: Predicate[D, L], doThen: PlanarDCELCutPipeline[D, L]) extends PlanarDCELCutPipeline[D, L]
  case class IfElse[D <: DCELData, L <: Labels](pred: Predicate[D, L], doThen: PlanarDCELCutPipeline[D, L], doElse: PlanarDCELCutPipeline[D, L]) extends PlanarDCELCutPipeline[D, L]
  case class Empty[D <: DCELData, L <: Labels]() extends PlanarDCELCutPipeline[D, L]
  case class Do[D <: DCELData, L <: Labels](code: CuttingContext[D, L] => Unit) extends PlanarDCELCutPipeline[D, L]

  case class ForEachFace[D <: DCELData, L <: Labels](selector: FaceSelector[D, L], doWith: Face[D] => Unit) extends PlanarDCELCutPipeline[D, L]
  case class ForEachEdge[D <: DCELData, L <: Labels](selector: EdgeSelector[D, L], doWith: HalfEdge[D] => Unit) extends PlanarDCELCutPipeline[D, L]
  case class ForEachVertex[D <: DCELData, L <: Labels](selector: VertexSelector[D, L], doWith: Vertex[D] => Unit) extends PlanarDCELCutPipeline[D, L]

  //ShapeCutting
  /*
  *                        rayEndOrHitPoint
  *                      ---------
  *                       / \
  *            lFace     /   rFace
  *            lEdge    /    rEdge == lEdge.twin
  *                    /     angleCCW from (at, ending) to (at, rayEndOrHitPoint)
  *      fHalf        /      sHalf
  *  *---------------*------------------->
  *  origin         at              ending
  *
  *  at = origin + (ending - origin) * ratio
   */
  case class TraceSegmentAtAngle[D <: DCELData, L <: Labels](edgeSelector: EdgeSelector[D, L],
                                                             ratioFromOrigin: Scalar,
                                                             maxLength: Scalar,
                                                             angleCCW: Scalar,
                                                             lFaceLabel: Option[L#FaceLabel] = None,
                                                             rFaceLabel: Option[L#FaceLabel] = None,
                                                             lEdgeLabel: Option[L#HalfEdgeLabel] = None,
                                                             rEdgeLabel: Option[L#HalfEdgeLabel] = None,
                                                             fHalfLabel: Option[L#HalfEdgeLabel] = None,
                                                             sHalfLabel: Option[L#HalfEdgeLabel] = None,
                                                             atLabel: Option[L#VertexLabel] = None,
                                                             rayEndOrHitPointLabel: Option[L#VertexLabel] = None,
                                                            ) extends PlanarDCELCutPipeline[D, L]

  case class CutPoly[D <: DCELData, L <: Labels](poly: PolygonRegion,
                                                 insideLabel: Option[L#FaceLabel] = None,
                                                 edgeLabels: Seq[L#HalfEdgeLabel] = Seq(),
                                                 edgeTwinLabels: Seq[L#HalfEdgeLabel] = Seq(),
                                                 vertexLabels: Seq[L#VertexLabel] = Seq()
                                                ) extends PlanarDCELCutPipeline[D, L]

  case class CutChain[D <: DCELData, L <: Labels](chain: Seq[V2],
                                                  edgeLabels: Seq[L#HalfEdgeLabel] = Seq(),
                                                  edgeTwinLabels: Seq[L#HalfEdgeLabel] = Seq(),
                                                  vertexLabels: Seq[L#VertexLabel] = Seq()
                                                 ) extends PlanarDCELCutPipeline[D, L]

  case class MergeFaces[D <: DCELData, L <: Labels](mainFaceSelector: SingleFaceSelector[D, L],
                                                    toMereFaceSelector: FaceSelector[D, L],
                                                    resultingFaceLabel: Option[L#FaceLabel] = None
                                                   ) extends PlanarDCELCutPipeline[D, L]

  /**
    *
    * @param centerVertexSelector
    * @param otherVertexSelector
    * @param edgeLabel     edge will be directed from centerVertex, to other
    * @param twinEdgeLabel edge will be directed from other, to centerVertex
    */
  case class ConnectVertices[D <: DCELData, L <: Labels](centerVertexSelector: SingleVertexSelector[D, L],
                                                         otherVertexSelector: VertexSelector[D, L],
                                                         edgeLabel: Option[L#HalfEdgeLabel] = None,
                                                         twinEdgeLabel: Option[L#HalfEdgeLabel] = None
                                                        ) extends PlanarDCELCutPipeline[D, L]


}
