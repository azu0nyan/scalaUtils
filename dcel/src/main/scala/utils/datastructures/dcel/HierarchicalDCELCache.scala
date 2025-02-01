package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL.{Face, HalfEdge, Vertex}
import utils.datastructures.dcel.HierarchicalDCEL.{HierarchicalEdge, HierarchicalFace, HierarchicalVertex, RHalfEdge}
import utils.datastructures.dcel.PlanarDCEL.PlanarVertex
import utils.math.planar.algo.polygonClipping.PolygonClipping
import utils.math.planar.{Polygon, PolygonRegion, SegmentPlanar}

import scala.collection.mutable

object HierarchicalDCELCache {
  /*
    Avoid using neighbours cached values as they can be incorrect.
  */


  def outerBorder[VOD, HOD, FOD](implicit face: HierarchicalFace[VOD, HOD, FOD]): Seq[RHalfEdge[VOD, HOD, FOD]] =
    face.face.borderEdges.toSeq

  def holesOwnContours[VOD, HOD, FOD](implicit face: HierarchicalFace[VOD, HOD, FOD]): Seq[Seq[RHalfEdge[VOD, HOD, FOD]]] = face.face.holesIncidentEdges.map(_.traverseEdges.toSeq).toSeq

  def fullBorder[VOD, HOD, FOD](implicit face: HierarchicalFace[VOD, HOD, FOD]): Seq[RHalfEdge[VOD, HOD, FOD]] = outerBorder ++ holesOwnContours.flatten

  /** Holes parent equals to my parent */
  def holeAreas[VOD, HOD, FOD](implicit face: HierarchicalFace[VOD, HOD, FOD]): Seq[HierarchicalFace[VOD, HOD, FOD]] = {
    //todo maybe this can be done separately, sine requires access to potentially uncached neighbours
    val visited: mutable.Set[HierarchicalFace[VOD, HOD, FOD]] = new mutable.HashSet[HierarchicalFace[VOD, HOD, FOD]]()

    def traverse(from: HierarchicalFace[VOD, HOD, FOD]): Unit =
      if (!visited.contains(from)) {
        visited += from
        for (a <- neighbourFaces(from) if a != face) traverse(a)
      }

    for (h <- face.face.holesEdges.map(_.twin.leftFace.data)) traverse(h)

    visited.toSeq
    //    area.face.holesIncidentEdges.map(_.twin.leftFace.data).toSeq
  }

  def neighbourFaces[VOD, HOD, FOD](implicit face: HierarchicalFace[VOD, HOD, FOD]): Seq[HierarchicalFace[VOD, HOD, FOD]] =
    fullBorder.map(_.twin.leftFace.data).distinct //todo
  //    fullBorder.filter(!_.twin.data.isFake).map(_.twin.leftFace.data).distinct

  /** inner Areas parent is me */
  def directChildFaces[VOD, HOD, FOD](implicit face: HierarchicalFace[VOD, HOD, FOD]): Seq[HierarchicalFace[VOD, HOD, FOD]] = face.innerDCEL.innerFaces.map(_.data).toSeq

  //todo check
  def allChildFaces[VOD, HOD, FOD](implicit face: HierarchicalFace[VOD, HOD, FOD]): Seq[HierarchicalFace[VOD, HOD, FOD]] = directChildFaces.flatMap(a => Seq(a) ++ allChildFaces(a)).toSeq


  def outerPoly[VOD, HOD, FOD](implicit face: HierarchicalFace[VOD, HOD, FOD]): PolygonRegion = PolygonRegion(outerBorder.map(x => x.origin.data.position))

  def holesPolys[VOD, HOD, FOD](implicit face: HierarchicalFace[VOD, HOD, FOD]): Seq[PolygonRegion] = holesOwnContours.map(h => PolygonRegion(h.map(x => x.origin.data.position)))

  def polygon[VOD, HOD, FOD](implicit face: HierarchicalFace[VOD, HOD, FOD]): Polygon = Polygon(outerPoly +: holesPolys) //todo maybe difference???

  def obstacles[VOD, HOD, FOD](implicit face: HierarchicalFace[VOD, HOD, FOD]): Seq[SegmentPlanar] = ownArea.regions.flatMap(_.sides) //todo cache

  //todo check if hole contours necessary( and then in navMesh)
  def ownArea[VOD, HOD, FOD](implicit face: HierarchicalFace[VOD, HOD, FOD]): Polygon = {
    val container = polygon
    val insides = holesPolys ++: face.innerDCEL.outerFace.holesContours.map(hc => PolygonRegion(hc.map(x => x.origin.data.position).toSeq)).toSeq
    val toCut = Polygon(insides)
    PolygonClipping.difference(container, toCut)
  }


  def apply[VOD, HOD, FOD](implicit area: HierarchicalFace[VOD, HOD, FOD]): HierarchicalDCELCacheInstance[VOD, HOD, FOD] = {
    HierarchicalDCELCacheInstance[VOD, HOD, FOD](
      outerBorder = outerBorder,
      holesOwnContours = holesOwnContours,
      fullBorder = fullBorder,
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

  case class HierarchicalDCELCacheInstance[VOD, HOD, FOD](
                                                           outerBorder: Seq[RHalfEdge[VOD, HOD, FOD]],
                                                           holesOwnContours: Seq[Seq[RHalfEdge[VOD, HOD, FOD]]],
                                                           fullBorder: Seq[RHalfEdge[VOD, HOD, FOD]],
                                                           holeAreas: Seq[HierarchicalFace[VOD, HOD, FOD]],
                                                           neighbourFaces: Seq[HierarchicalFace[VOD, HOD, FOD]],
                                                           directChildFaces: Seq[HierarchicalFace[VOD, HOD, FOD]],
                                                           allChildFaces: Seq[HierarchicalFace[VOD, HOD, FOD]],
                                                           outerPoly: PolygonRegion,
                                                           holesPolys: Seq[PolygonRegion],
                                                           polygon: Polygon,
                                                           ownArea: Polygon,
                                                           obstacles: Seq[SegmentPlanar]
                                                         ) {

  }
}



