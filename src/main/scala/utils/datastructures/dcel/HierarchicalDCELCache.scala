package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL.{RawFace, RawHalfEdge, RawVertex}
import utils.datastructures.dcel.HierarchicalDCEL.{HierarchicalEdge, HierarchicalFace, HierarchicalVertex}
import utils.datastructures.dcel.PlanarDCEL.PlanarVertex
import utils.math.planar.algo.polygonClipping.PolygonClipping
import utils.math.planar.{Polygon, PolygonRegion, SegmentPlanar}

import scala.collection.mutable

object HierarchicalDCELCache {
  /*
    Avoid using neighbours cached values as they can be incorrect.
  */

  //  type Face[V, HE, F] = RawFace[HierarchicalVertex[V, HE, F],  HierarchicalEdge[V, HE, F],  HierarchicalFace[V, HE, F]]
  //  type Vertex[V, HE, F] = RawVertex[HierarchicalVertex[V, HE, F],  HierarchicalEdge[V, HE, F],  HierarchicalFace[V, HE, F]]
  type HalfEdge[V, HE, F] = RawHalfEdge[HierarchicalVertex[V, HE, F],  HierarchicalEdge[V, HE, F],  HierarchicalFace[V, HE, F]]

  def outerBorder[V, HE, F](implicit face: HierarchicalFace[V, HE, F]): Seq[HalfEdge[V, HE, F]] =
    face.face.borderEdges.toSeq
  def holesOwnContours[V, HE, F](implicit face: HierarchicalFace[V, HE, F]): Seq[Seq[HalfEdge[V, HE, F]]] = face.face.holesIncidentEdges.map(_.traverseEdges.toSeq).toSeq
  def fullBorder[V, HE, F](implicit face: HierarchicalFace[V, HE, F]): Seq[HalfEdge[V, HE, F]] = outerBorder ++ holesOwnContours.flatten

  /** Holes parent equals to my parent */
  def holeAreas[V, HE, F](implicit face: HierarchicalFace[V, HE, F]): Seq[HierarchicalFace[V, HE, F] ] = {
    //todo maybe this can be done separately, sine requires access to potentially uncached neighbours
    val visited: mutable.Set[HierarchicalFace[V, HE, F] ] = new mutable.HashSet[HierarchicalFace[V, HE, F] ]()
    def traverse(from: HierarchicalFace[V, HE, F]): Unit =
      if(!visited.contains(from)){
        visited += from
        for(a <- neighbourFaces(from) if a != face) traverse(a)
      }

    for(h <- face.face.holesEdges.map(_.twin.leftFace.data)) traverse(h)

    visited.toSeq
    //    area.face.holesIncidentEdges.map(_.twin.leftFace.data).toSeq
  }
  def neighbourFaces[V, HE, F](implicit face: HierarchicalFace[V, HE, F]): Seq[HierarchicalFace[V, HE, F] ] =
    fullBorder.map(_.twin.leftFace.data).distinct //todo
//    fullBorder.filter(!_.twin.data.isFake).map(_.twin.leftFace.data).distinct

  /** inner Areas parent is me */
  def directChildFaces[V, HE, F](implicit face: HierarchicalFace[V, HE, F]): Seq[HierarchicalFace[V, HE, F] ] = face.innerDCEL.innerFaces.map(_.data).toSeq
  //todo check
  def allChildFaces[V, HE, F](implicit face: HierarchicalFace[V, HE, F]): Seq[HierarchicalFace[V, HE, F] ] = directChildFaces.flatMap(a => Seq(a) ++ allChildFaces(a)).toSeq


  def outerPoly[V, HE, F](implicit face: HierarchicalFace[V, HE, F]): PolygonRegion = PolygonRegion(outerBorder.map(x => x.origin.data.position))
  def holesPolys[V, HE, F](implicit face: HierarchicalFace[V, HE, F]): Seq[PolygonRegion] = holesOwnContours.map(h => PolygonRegion(h.map(x => x.origin.data.position)))
  def polygon[V, HE, F](implicit face: HierarchicalFace[V, HE, F]): Polygon = Polygon(outerPoly +: holesPolys) //todo maybe difference???
  def obstacles[V, HE, F](implicit face: HierarchicalFace[V, HE, F]): Seq[SegmentPlanar] = ownArea.regions.flatMap(_.sides) //todo cache

  //todo check if hole contours necessary
  def ownArea[V, HE, F](implicit face: HierarchicalFace[V, HE, F]): Polygon = {
    val container = polygon
    val insides = holesPolys ++: face.innerDCEL.outerFace.holesContours.map(hc => PolygonRegion(hc.map(x => x.origin.data.position).toSeq)).toSeq
    val toCut = Polygon(insides)
    PolygonClipping.difference(container, toCut)
  }



  def apply[V, HE, F](implicit area: HierarchicalFace[V, HE, F]): HierarchicalDCELCacheInstance[V, HE, F] = {
    HierarchicalDCELCacheInstance[V, HE, F](
      outerBorder = outerBorder,
      holesOwnContours = holesOwnContours,
      fullBorder= fullBorder,
      holeAreas = holeAreas,
      neighbourFaces = neighbourFaces,
      directChildFaces = directChildFaces,
      allChildFaces = allChildFaces,
      outerPoly = outerPoly,
      holesPolys = holesPolys,
      polygon = polygon,
      ownArea = ownArea,
      obstacles = obstacles
    )
  }
  case class HierarchicalDCELCacheInstance[V, HE, F](
                                                      outerBorder: Seq[HalfEdge[V, HE, F]],
                                                      holesOwnContours: Seq[Seq[HalfEdge[V, HE, F]]],
                                                      fullBorder: Seq[HalfEdge[V, HE, F]],
                                                      holeAreas: Seq[HierarchicalFace[V, HE, F] ],
                                                      neighbourFaces: Seq[HierarchicalFace[V, HE, F] ],
                                                      directChildFaces: Seq[HierarchicalFace[V, HE, F] ],
                                                      allChildFaces: Seq[HierarchicalFace[V, HE, F] ],
                                                      outerPoly: PolygonRegion,
                                                      holesPolys: Seq[PolygonRegion],
                                                      polygon: Polygon,
                                                      ownArea: Polygon,
                                                      obstacles: Seq[SegmentPlanar]
                                            ) {

  }
}



