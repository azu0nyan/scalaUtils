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

  sealed trait BorderNode extends PathNode

  case class PointNode(point: V2, area: NavigableFace) extends PathNode

  case class FreeBorderNode(border: NavigableHalfEdge, startFraction: Scalar, endFraction: Scalar) extends BorderNode {
    override def point: V2 = border.hierarchicalEdge.asSegment.sampleAt((startFraction + endFraction) / 2d)
    override def area: NavigableFace = border.hierarchicalEdge.face.data.ownData //area

    def twinNode: Option[FreeBorderNode] = {
      val twin = border.edgeNodeTwin
      twin.ownPathNodes.collectFirst { case e: FreeBorderNode => e }
    }
//    override def toString: String = s"EdgeNode(${border.name})"
  }

  case class PortalNode(from: NavigableHalfEdge, portal: Portal) extends BorderNode {
    override def area: NavigableFace = from.area
    override def point: V2 = if (portal.from == from) portal.fromPoint else portal.toPoint
    def travelsToArea: NavigableFace = travelsToBorder.area
    def toPoint: V2 = if (portal.from == from) portal.toPoint else portal.fromPoint
    def travelsToEdge: NavigableHalfEdge = if (portal.from == from) portal.to else portal.from
    def travelsToBorder: NavigableHalfEdge = if (portal.from == from) portal.to else portal.from

    //todo if two sided
    def twinNode: PortalNode = PortalNode(travelsToBorder, portal)

//    override def toString: String = s"PortalNode(from = ${from.name}, to = ${travelsToBorder.name})"
  }


  sealed trait PathEdge[+FROM <: PathNode, +TO <: PathNode] {
    def from: FROM
    def to: TO
    def length: Scalar
    def reverse: PathEdge[TO, FROM]
    //    def next:Option[PathEdge[TO, _]]
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

  case class GoTroughPortal(from: PortalNode, to: PortalNode) extends PathEdge[PortalNode, PortalNode] {
    override val length: Scalar = 0
    override def reverse: GoTroughPortal = GoTroughPortal(to, from)
  }

  case class GoTroughAreaOnVisGraph(from: BorderNode, to: BorderNode, a: NavigableFace, length: Scalar, calculatedPath: Option[DCELPath]) extends PathEdge[BorderNode, BorderNode] {
    override def reverse: GoTroughAreaOnVisGraph = GoTroughAreaOnVisGraph(to, from, a, length, calculatedPath.map(_.reverse))
  }

  case class GoTroughArea(from: BorderNode, to: BorderNode, a: NavigableFace, length: Scalar, calculatedPath: Option[DCELPath]) extends PathEdge[BorderNode, BorderNode] {
    override def reverse: GoTroughArea = GoTroughArea(to, from, a, length, calculatedPath.map(_.reverse))
  }

  case class GoAlongSameBorder(from: BorderNode, to: BorderNode) extends PathEdge[BorderNode, BorderNode] {
    override val length: Scalar = from.point.distance(to.point)
    override def reverse: GoAlongSameBorder = GoAlongSameBorder(to, from)
  }

  case class GoTroughBorder(from: FreeBorderNode, to: FreeBorderNode) extends PathEdge[FreeBorderNode, FreeBorderNode] {
    override val length: Scalar = 0
    override def reverse: PathEdge[FreeBorderNode, FreeBorderNode] = GoTroughBorder(to, from)
  }

  case class GoToParentBorder(from: FreeBorderNode, to: FreeBorderNode) extends PathEdge[FreeBorderNode, FreeBorderNode] {
    override val length: Scalar = from.point.distance(to.point)
    override def reverse: GoToChildBorder = GoToChildBorder(to, from)
  }

  case class GoToChildBorder(from: FreeBorderNode, to: FreeBorderNode) extends PathEdge[FreeBorderNode, FreeBorderNode] {
    override val length: Scalar = from.point.distance(to.point)
    override def reverse: GoToParentBorder = GoToParentBorder(to, from)
  }

  def checkConnectionCorrectness(edges: Seq[PathEdge[_, _]]): Boolean =
    edges.size < 2 || edges.sliding(2).forall { case Seq(e1, e2) => e1.to == e2.from }


  //  type VisibilityGraphPath = DCELPath[PointNode]
  //  type EdgeGraphPath = DCELPath[BorderNode]
  //  type FullPath = DCELPath[PathNode]
  case class DCELPath(
                       edges: Seq[PathEdge[PathNode, PathNode]]
                     ) {
    def replaceHead(points: Seq[PathEdge[PathNode, PathNode]]): DCELPath = DCELPath(points ++ edges.tail)

    def appendNode(toNode: PathNode): DCELPath =
      toNode match {
        case p: PortalNode =>
          edges.last.to match {
            case pn: PortalNode => throw new NotImplementedError()
            case bn: FreeBorderNode => throw new NotImplementedError()
            case pn: PointNode => appendEdge(GoFromPoint(pn, p))
          }
        case b: FreeBorderNode =>
          edges.last.to match {
            case pn: PortalNode => throw new NotImplementedError()
            case bn: FreeBorderNode => throw new NotImplementedError()
            case pn: PointNode => appendEdge(GoFromPoint(pn, b))
          }
        case p: PointNode => appendPointNode(p)
      }


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
    def appendPointNode(node: PointNode): DCELPath =
      DCELPath(edges :+ (edges.last.to match {
        case b: FreeBorderNode => GoToPoint(b, node)
        case p: PointNode => GoBetweenPoints(p, node)
        case _ => throw new NotImplementedError(s"Last node type not supported yet ${edges.last.to}")
      }))

    def reverse: DCELPath = DCELPath(edges.reverse.map(_.reverse))
  }
  object DCELPath {
    def fromPath(path: Path[PathNode, PathEdge[PathNode, PathNode]]): DCELPath =
      DCELPath(path.fromByTo.map(_._2))
    //    def fromPoints(values: Seq[PointNode]): DCELPath = if(values){
    //
    //    }

    //    def fromLeftWaypoints(waypoints: Seq[(PathNode, Scalar)]): Option[DCELPath] =
    //      Option.when(waypoints.nonEmpty)(DCELPath(waypoints.head._1, waypoints.tail))
    //
    //    def fromPath(p: Path[PathNode, Scalar]): DCELPath = DCELPath(p.start, p.otherNodes.map(n => (n.to, n.by)))
  }
}
