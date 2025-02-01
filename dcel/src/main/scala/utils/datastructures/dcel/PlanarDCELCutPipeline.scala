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


  sealed trait PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL] {

  }

  case class CuttingContext[VD, HD, FD, VL, HL, FL](dcel: PlanarDCEL[VD, HD, FD],
                                                        provider: DCELDataProvider[VD, HD, FD],
                                                        vertexProduced: Set[Vertex[VD, HD, FD]] = Set[Vertex[VD, HD, FD]](),
                                                        halfEdgesProduced: Set[HalfEdge[VD, HD, FD]] = Set[HalfEdge[VD, HD, FD]](),
                                                        faceProduced: Set[Face[VD, HD, FD]] = Set[Face[VD, HD, FD]](),
                                                        labelToHalfEdge: Map[HL, Set[HalfEdge[VD, HD, FD]]] = Map[HL, Set[HalfEdge[VD, HD, FD]]](),
                                                        labelToFace: Map[FL, Set[Face[VD, HD, FD]]] = Map[FL, Set[Face[VD, HD, FD]]](),
                                                        labelToVertex: Map[VL, Set[Vertex[VD, HD, FD]]] = Map[VL, Set[Vertex[VD, HD, FD]]]()) {
    def addVertices(vertices: Iterable[Vertex[VD, HD, FD]]): CuttingContext[VD, HD, FD, VL, HL, FL] = copy(vertexProduced = vertexProduced | vertices.toSet)
    def addFaces(areas: Iterable[Face[VD, HD, FD]]): CuttingContext[VD, HD, FD, VL, HL, FL] = copy(faceProduced = faceProduced | areas.toSet)
    def addHalfEdges(edges: Iterable[HalfEdge[VD, HD, FD]]): CuttingContext[VD, HD, FD, VL, HL, FL] = copy(halfEdgesProduced = halfEdgesProduced | edges.toSet)

    def removeVertices(vertices: Iterable[Vertex[VD, HD, FD]]): CuttingContext[VD, HD, FD, VL, HL, FL] = copy(vertexProduced = vertexProduced &~ vertices.toSet)
    def removeFaces(areas: Iterable[Face[VD, HD, FD]]): CuttingContext[VD, HD, FD, VL, HL, FL] = copy(faceProduced = faceProduced &~ areas.toSet)
    def removeHalfEdges(edges: Iterable[HalfEdge[VD, HD, FD]]): CuttingContext[VD, HD, FD, VL, HL, FL] = copy(halfEdgesProduced = halfEdgesProduced &~ edges.toSet)

    def addEdgeLabels(labels: Iterable[(HL, HalfEdge[VD, HD, FD])]): CuttingContext[VD, HD, FD, VL, HL, FL] = copy(labelToHalfEdge = MapOps.addSeqToMapToSet(labelToHalfEdge, labels.toSeq))
    def addFaceLabels(labels: Iterable[(FL, Face[VD, HD, FD])]): CuttingContext[VD, HD, FD, VL, HL, FL] = copy(labelToFace = MapOps.addSeqToMapToSet(labelToFace, labels.toSeq))
    def addVertexLabels(labels: Iterable[(VL, Vertex[VD, HD, FD])]): CuttingContext[VD, HD, FD, VL, HL, FL] = copy(labelToVertex = MapOps.addSeqToMapToSet(labelToVertex, labels.toSeq))
    //maybe store in reverse map to speed up??
    def vertexToLabel(v: Vertex[VD, HD, FD]): Set[VL] = labelToVertex.filter(_._2.contains(v)).keySet
    def halfEdgeToLabel(h: HalfEdge[VD, HD, FD]): Set[HL] = labelToHalfEdge.filter(_._2.contains(h)).keySet
    def faceToLabel(f: Face[VD, HD, FD]): Set[FL] = labelToFace.filter(_._2.contains(f)).keySet

  }

  type SingleFaceSelector[VD, HD, FD, VL, HL, FL] = CuttingContext[VD, HD, FD, VL, HL, FL] => Option[Face[VD, HD, FD]]
  type FaceSelector[VD, HD, FD, VL, HL, FL] = CuttingContext[VD, HD, FD, VL, HL, FL] => Seq[Face[VD, HD, FD]]
  type SingleEdgeSelector[VD, HD, FD, VL, HL, FL] = CuttingContext[VD, HD, FD, VL, HL, FL] => Option[HalfEdge[VD, HD, FD]]
  type EdgeSelector[VD, HD, FD, VL, HL, FL] = CuttingContext[VD, HD, FD, VL, HL, FL] => Seq[HalfEdge[VD, HD, FD]]
  type SingleVertexSelector[VD, HD, FD, VL, HL, FL] = CuttingContext[VD, HD, FD, VL, HL, FL] => Option[Vertex[VD, HD, FD]]
  type VertexSelector[VD, HD, FD, VL, HL, FL] = CuttingContext[VD, HD, FD, VL, HL, FL] => Seq[Vertex[VD, HD, FD]]
  type Predicate[VD, HD, FD, VL, HL, FL] = CuttingContext[VD, HD, FD, VL, HL, FL] => Boolean

  //Structural
  case class ProducePipeline[VD, HD, FD, VL, HL, FL](producer: CuttingContext[VD, HD, FD, VL, HL, FL] => PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL]) extends PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL]
  case class TransformContext[VD, HD, FD, VL, HL, FL](transformer: CuttingContext[VD, HD, FD, VL, HL, FL] => CuttingContext[VD, HD, FD, VL, HL, FL]) extends PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL]
  case class StepsSeq[VD, HD, FD, VL, HL, FL](steps: PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL]*) extends PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL]
  case class If[VD, HD, FD, VL, HL, FL](pred: Predicate[VD, HD, FD, VL, HL, FL], doThen: PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL]) extends PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL]
  case class IfElse[VD, HD, FD, VL, HL, FL](pred: Predicate[VD, HD, FD, VL, HL, FL], doThen: PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL], doElse: PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL]) extends PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL]
  case class Empty[VD, HD, FD, VL, HL, FL]() extends PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL]
  case class Do[VD, HD, FD, VL, HL, FL](code: CuttingContext[VD, HD, FD, VL, HL, FL] => Unit) extends PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL]

  case class ForEachFace[VD, HD, FD, VL, HL, FL](selector: FaceSelector[VD, HD, FD, VL, HL, FL], doWith: Face[VD, HD, FD] => Unit) extends PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL]
  case class ForEachEdge[VD, HD, FD, VL, HL, FL](selector: EdgeSelector[VD, HD, FD, VL, HL, FL], doWith: HalfEdge[VD, HD, FD] => Unit) extends PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL]
  case class ForEachVertex[VD, HD, FD, VL, HL, FL](selector: VertexSelector[VD, HD, FD, VL, HL, FL], doWith: Vertex[VD, HD, FD] => Unit) extends PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL]

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
  case class TraceSegmentAtAngle[VD, HD, FD, VL, HL, FL](edgeSelector: EdgeSelector[VD, HD, FD, VL, HL, FL],
                                                             ratioFromOrigin: Scalar,
                                                             maxLength: Scalar,
                                                             angleCCW: Scalar,
                                                             lFaceLabel: Option[FL] = None,
                                                             rFaceLabel: Option[FL] = None,
                                                             lEdgeLabel: Option[HL] = None,
                                                             rEdgeLabel: Option[HL] = None,
                                                             fHalfLabel: Option[HL] = None,
                                                             sHalfLabel: Option[HL] = None,
                                                             atLabel: Option[VL] = None,
                                                             rayEndOrHitPointLabel: Option[VL] = None,
                                                            ) extends PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL]

  case class CutPoly[VD, HD, FD, VL, HL, FL](poly: PolygonRegion,
                                                 insideLabel: Option[FL] = None,
                                                 edgeLabels: Seq[HL] = Seq(),
                                                 edgeTwinLabels: Seq[HL] = Seq(),
                                                 vertexLabels: Seq[VL] = Seq()
                                                ) extends PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL]

  case class CutChain[VD, HD, FD, VL, HL, FL](chain: Seq[V2],
                                                  edgeLabels: Seq[HL] = Seq(),
                                                  edgeTwinLabels: Seq[HL] = Seq(),
                                                  vertexLabels: Seq[VL] = Seq()
                                                 ) extends PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL]

  case class MergeFaces[VD, HD, FD, VL, HL, FL](mainFaceSelector: SingleFaceSelector[VD, HD, FD, VL, HL, FL],
                                                    toMereFaceSelector: FaceSelector[VD, HD, FD, VL, HL, FL],
                                                    resultingFaceLabel: Option[FL] = None
                                                   ) extends PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL]

  /**
    *
    * @param centerVertexSelector
    * @param otherVertexSelector
    * @param edgeLabel     edge will be directed from centerVertex, to other
    * @param twinEdgeLabel edge will be directed from other, to centerVertex
    */
  case class ConnectVertices[VD, HD, FD, VL, HL, FL](centerVertexSelector: SingleVertexSelector[VD, HD, FD, VL, HL, FL],
                                                         otherVertexSelector: VertexSelector[VD, HD, FD, VL, HL, FL],
                                                         edgeLabel: Option[HL] = None,
                                                         twinEdgeLabel: Option[HL] = None
                                                        ) extends PlanarDCELCutPipeline[VD, HD, FD, VL, HL, FL]


}
