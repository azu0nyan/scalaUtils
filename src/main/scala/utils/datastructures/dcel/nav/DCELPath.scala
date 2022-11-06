package utils.datastructures.dcel.nav

import utils.datastructures.dcel.nav.NavigableDCEL.{NavigableFace, NavigableHalfEdge}
import utils.datastructures.dcel.nav.Portal.Portal
import utils.datastructures.graph.Graph.Path
import utils.math.Scalar
import utils.math.planar.V2

object DCELPath {

  sealed trait PathNode {
    def point: V2
    def area: NavigableFace
  }

  case class PointNode(point: V2, area: NavigableFace) extends PathNode

  case class BorderNode(border: NavigableHalfEdge, startFraction: Scalar, endFraction: Scalar) extends PathNode {
    override def point: V2 = border.hierarchicalEdge.asSegment.sampleAt((startFraction + endFraction) / 2d)
    override def area: NavigableFace = border.hierarchicalEdge.face.data.ownData //area
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
