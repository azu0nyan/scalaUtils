package utils.datastructures.dcel.nav

import utils.datastructures.dcel.DCEL.{Face, HalfEdge}
import utils.datastructures.dcel.PlanarDCEL.PlanarEdge
import utils.datastructures.dcel.nav.NavMesh.{NavMeshDcelData, NavMeshPosition}
import utils.datastructures.graph.Graph.Path
import utils.math._
import utils.math.WithAlmostEquals
import utils.math.misc.IntervalOps
import utils.math.planar.V2

object DCELPath {

  sealed trait PathNode {
    def point: V2
    def face: NavigableFace
  }

  case class NavMeshPosition(point: V2, face: NavigableFace, navMeshFace: Face[NavMeshDcelData]) extends  PathNode

  case class NavMeshEdge(edge: HalfEdge[NavMeshDcelData]) extends PathNode {
    override def point: V2 = edge.asSegment.center
    override def face: NavigableFace = ???
  }

  case class BorderNode(border: NavigableHalfEdge, startFraction: Scalar, endFraction: Scalar) extends PathNode {
    override def point: V2 = border.hierarchicalEdge.asSegment.sampleAt((startFraction + endFraction) / 2d)
    override def face: NavigableFace = border.hierarchicalEdge.face.data.ownData //area


    def nodeLength: Scalar = startPoint.distance(endPoint)
    def startPoint: V2 = border.hierarchicalEdge.asSegment.sampleAt(startFraction)
    def endPoint: V2 = border.hierarchicalEdge.asSegment.sampleAt(endFraction)


    def overlapLength(ot: BorderNode): Scalar = commonNonemptyInterval(ot) match {
      case Some((l, r)) => border.hierarchicalEdge.asSegment.sampleAt(l).distance(border.hierarchicalEdge.asSegment.sampleAt(r))
      case None => 0d
    }

    /**Returns common interval in fractions of border's segment, assumes that ot is collinear(but no assumption on same direction)
      * first fraction will be less than second
      * return
      * */
    def commonNonemptyInterval(ot:BorderNode): Option[(Scalar, Scalar)] = {
      val ef = clamp(border.hierarchicalEdge.asSegment.getFractionAt(ot.startPoint), 0, 1)
      val sf = clamp(border.hierarchicalEdge.asSegment.getFractionAt(ot.endPoint), 0, 1)
      if(ef ~< sf) Some((ef, sf))
      else if(ef ~> sf) Some((sf, ef))
      else None
    }

    /**Includes twin, twin's child's and twin's parent nodes*/
    def allOppositeNodes(minLength: Scalar): Seq[BorderNode] = {
      border.hierarchicalEdge.meAndAllLevelChilds.flatMap(e => e.ownData.borderNodes).filter(_.overlapLength(this) >= minLength)
    }

    /**Twin's opposite nodes*/
    def oppositeNodes(minLength: Scalar): Seq[BorderNode] = {
      border.hierarchicalEdge.ownData.edgeNodeTwin.borderNodes.filter(_.overlapLength(this) >= minLength)
    }

    /** Return true if exists segment that nonOverlapped by any child with specified minimum length*/
    def existsNonChildSegment(minLength: Scalar): Boolean = {
      val toCut = border.hierarchicalEdge.allLevelChilds.flatMap(c => c.ownData.borderNodes).map(bn => (
        border.hierarchicalEdge.asSegment.getFractionAt(bn.startPoint),
        border.hierarchicalEdge.asSegment.getFractionAt(bn.endPoint),
      ))

      IntervalOps.cutFrom((startFraction, endFraction), toCut).exists{case (l, r) => (r - l) >= minLength}
    }

  }


  sealed trait PathEdge[+FROM <: PathNode, +TO <: PathNode] {
    def from: FROM
    def to: TO
    def length: Scalar
    def reverse: PathEdge[TO, FROM]
  }

  type BetweenBordersEdge = PathEdge[BorderNode, BorderNode]

  case class GoBetweenPoints(from: PointNode, to: PointNode) extends PathEdge[PointNode, PointNode] {
    override val length: Scalar = from.point.distance(to.point)
    override def reverse: GoBetweenPoints = GoBetweenPoints(to, from)
  }

  case class GoToPoint(from: BorderNode, to: PointNode) extends PathEdge[BorderNode, PointNode] {
    override val length: Scalar = from.point.distance(to.point)
    override def reverse: GoFromPoint = GoFromPoint(to, from)
  }

  case class GoFromPoint(from: PointNode, to: BorderNode) extends PathEdge[PointNode, BorderNode] {
    override val length: Scalar = from.point.distance(to.point)
    override def reverse: GoToPoint = GoToPoint(to, from)
  }

  case class GoTroughArea(from: BorderNode, to: BorderNode, a: NavigableFace, length: Scalar, calculatedPath: Option[DCELPath]) extends PathEdge[BorderNode, BorderNode] {
    override def reverse: GoTroughArea = GoTroughArea(to, from, a, length, calculatedPath.map(_.reverse))
  }

  case class GoTroughBorder(from: BorderNode, to: BorderNode) extends PathEdge[BorderNode, BorderNode] {
    override val length: Scalar = 0
    override def reverse: PathEdge[BorderNode, BorderNode] = GoTroughBorder(to, from)
  }

  case class GoToParent(from: BorderNode, to: BorderNode) extends PathEdge[BorderNode, BorderNode] {
    override val length: Scalar = from.point.distance(to.point)
    override def reverse: GoToChild = GoToChild(to, from)
  }

  case class GoToChild(from: BorderNode, to: BorderNode) extends PathEdge[BorderNode, BorderNode] {
    override val length: Scalar = from.point.distance(to.point)
    override def reverse: GoToParent = GoToParent(to, from)
  }


  //  case class GoTroughAreaOnVisGraph(from: BorderNode, to: BorderNode, a: NavigableFace, length: Scalar, calculatedPath: Option[DCELPath]) extends PathEdge[BorderNode, BorderNode] {
  //    override def reverse: GoTroughAreaOnVisGraph = GoTroughAreaOnVisGraph(to, from, a, length, calculatedPath.map(_.reverse))
  //  }

  //  case class GoAlongSameBorder(from: BorderNode, to: BorderNode) extends PathEdge[BorderNode, BorderNode] {
  //    override val length: Scalar = from.point.distance(to.point)
  //    override def reverse: GoAlongSameBorder = GoAlongSameBorder(to, from)
  //  }

  def checkConnectionCorrectness(edges: Seq[PathEdge[_, _]]): Boolean =
    edges.size < 2 || edges.sliding(2).forall { case Seq(e1, e2) => e1.to == e2.from }


  //  type VisibilityGraphPath = DCELPath[PointNode]
  //  type EdgeGraphPath = DCELPath[BorderNode]
  //  type FullPath = DCELPath[PathNode]
  case class DCELPath(
                       edges: Seq[PathEdge[PathNode, PathNode]]
                     ) {
    def replaceHead(points: Seq[PathEdge[PathNode, PathNode]]): DCELPath = DCELPath(points ++ edges.tail)

    //    def appendNode(toNode: PathNode): DCELPath =
    //      toNode match {
    //        case b: BorderNode =>
    //          edges.last.to match {
    //            case bn: BorderNode => throw new NotImplementedError()
    //            case pn: PointNode => appendEdge(GoFromPoint(pn, b))
    //          }
    //        case p: PointNode => appendPointNode(p)
    //      }


    def appendEdge(edge: PathEdge[PathNode, PathNode]): DCELPath = DCELPath(edges :+ edge)


    def dropFirstNode: Option[DCELPath] =
      Option.when(edges.size > 1)(DCELPath(edges.tail))

    lazy val waypoints: Seq[PathNode] = {
      if (edges.nonEmpty) edges.head.from.asInstanceOf[PathNode] +: edges.tail.map(_.to.asInstanceOf[PathNode])
      else Seq()
    }

    lazy val length: Scalar = edges.map(_.length).sum

    def appendPath(path: DCELPath, connector: Option[PathEdge[PathNode, PathNode]]): DCELPath = connector match {
      case Some(value) =>
        DCELPath((edges :+ value) ++ path.edges)
      case None =>
        DCELPath(edges ++ path.edges)
    }

    //    def prependNode(node: PathNode, len: Scalar): DCELPath = DCELPath(node, (start, len) +: waypointsWithLength)
    //
    //    def appendNode(node: PathNode, len: Scalar): DCELPath = DCELPath(start, waypointsWithLength :+ (node, len))
    //    def appendPointNode(node: PointNode): DCELPath =
    //      DCELPath(edges :+ (edges.last.to match {
    //        case b: BorderNode => GoToPoint(b, node)
    //        case p: PointNode => GoBetweenPoints(p, node)
    //        case _ => throw new NotImplementedError(s"Last node type not supported yet ${edges.last.to}")
    //      }))

    def reverse: DCELPath = DCELPath(edges.reverse.map(_.reverse))
  }
  object DCELPath {
    def fromPath(path: Path[PathNode, PathEdge[PathNode, PathNode]]): DCELPath =
      DCELPath(path.fromByTo.map(_._2))
    //    def fromPoints(values: Seq[PointNode]): DCELPath = if(values){
    //
        }


}
