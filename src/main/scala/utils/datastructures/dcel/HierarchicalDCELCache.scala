package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL.{Face, HalfEdge, Vertex}
import utils.datastructures.dcel.HierarchicalDCEL.{HierarchicalDCELData, HierarchicalDCELOwnData, HierarchicalEdge, HierarchicalFace, HierarchicalVertex, RHalfEdge}
import utils.datastructures.dcel.PlanarDCEL.PlanarVertex
import utils.math.planar.algo.polygonClipping.PolygonClipping
import utils.math.planar.{Polygon, PolygonRegion, SegmentPlanar}

import scala.collection.mutable

object HierarchicalDCELCache {
  /*
    Avoid using neighbours cached values as they can be incorrect.
  */


  def outerBorder[OD <: HierarchicalDCELOwnData](implicit face: HierarchicalFace[OD]): Seq[RHalfEdge[OD]] =
    face.face.borderEdges.toSeq

  def holesOwnContours[OD <: HierarchicalDCELOwnData](implicit face: HierarchicalFace[OD]): Seq[Seq[RHalfEdge[OD]]] = face.face.holesIncidentEdges.map(_.traverseEdges.toSeq).toSeq

  def fullBorder[OD <: HierarchicalDCELOwnData](implicit face: HierarchicalFace[OD]): Seq[RHalfEdge[OD]] = outerBorder ++ holesOwnContours.flatten

  /** Holes parent equals to my parent */
  def holeAreas[OD <: HierarchicalDCELOwnData](implicit face: HierarchicalFace[OD]): Seq[HierarchicalFace[OD]] = {
    //todo maybe this can be done separately, sine requires access to potentially uncached neighbours
    val visited: mutable.Set[HierarchicalFace[OD]] = new mutable.HashSet[HierarchicalFace[OD]]()

    def traverse(from: HierarchicalFace[OD]): Unit =
      if (!visited.contains(from)) {
        visited += from
        for (a <- neighbourFaces(from) if a != face) traverse(a)
      }

    for (h <- face.face.holesEdges.map(_.twin.leftFace.data)) traverse(h)

    visited.toSeq
    //    area.face.holesIncidentEdges.map(_.twin.leftFace.data).toSeq
  }

  def neighbourFaces[OD <: HierarchicalDCELOwnData](implicit face: HierarchicalFace[OD]): Seq[HierarchicalFace[OD]] =
    fullBorder.map(_.twin.leftFace.data).distinct //todo
  //    fullBorder.filter(!_.twin.data.isFake).map(_.twin.leftFace.data).distinct

  /** inner Areas parent is me */
  def directChildFaces[OD <: HierarchicalDCELOwnData](implicit face: HierarchicalFace[OD]): Seq[HierarchicalFace[OD]] = face.innerDCEL.innerFaces.map(_.data).toSeq

  //todo check
  def allChildFaces[OD <: HierarchicalDCELOwnData](implicit face: HierarchicalFace[OD]): Seq[HierarchicalFace[OD]] = directChildFaces.flatMap(a => Seq(a) ++ allChildFaces(a)).toSeq


  def outerPoly[OD <: HierarchicalDCELOwnData](implicit face: HierarchicalFace[OD]): PolygonRegion = PolygonRegion(outerBorder.map(x => x.origin.data.position))

  def holesPolys[OD <: HierarchicalDCELOwnData](implicit face: HierarchicalFace[OD]): Seq[PolygonRegion] = holesOwnContours.map(h => PolygonRegion(h.map(x => x.origin.data.position)))

  def polygon[OD <: HierarchicalDCELOwnData](implicit face: HierarchicalFace[OD]): Polygon = Polygon(outerPoly +: holesPolys) //todo maybe difference???

  def obstacles[OD <: HierarchicalDCELOwnData](implicit face: HierarchicalFace[OD]): Seq[SegmentPlanar] = ownArea.regions.flatMap(_.sides) //todo cache

  //todo check if hole contours necessary( and then in navMesh)
  def ownArea[OD <: HierarchicalDCELOwnData](implicit face: HierarchicalFace[OD]): Polygon = {
    val container = polygon
    val insides = holesPolys ++: face.innerDCEL.outerFace.holesContours.map(hc => PolygonRegion(hc.map(x => x.origin.data.position).toSeq)).toSeq
    val toCut = Polygon(insides)
    PolygonClipping.difference(container, toCut)
  }


  def apply[OD <: HierarchicalDCELOwnData](implicit area: HierarchicalFace[OD]): HierarchicalDCELCacheInstance[OD] = {
    HierarchicalDCELCacheInstance[OD](
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

  case class HierarchicalDCELCacheInstance[OD <: HierarchicalDCELOwnData](
                                                                           outerBorder: Seq[RHalfEdge[OD]],
                                                                           holesOwnContours: Seq[Seq[RHalfEdge[OD]]],
                                                                           fullBorder: Seq[RHalfEdge[OD]],
                                                                           holeAreas: Seq[HierarchicalFace[OD]],
                                                                           neighbourFaces: Seq[HierarchicalFace[OD]],
                                                                           directChildFaces: Seq[HierarchicalFace[OD]],
                                                                           allChildFaces: Seq[HierarchicalFace[OD]],
                                                                           outerPoly: PolygonRegion,
                                                                           holesPolys: Seq[PolygonRegion],
                                                                           polygon: Polygon,
                                                                           ownArea: Polygon,
                                                                           obstacles: Seq[SegmentPlanar]
                                                                         ) {

  }
}



