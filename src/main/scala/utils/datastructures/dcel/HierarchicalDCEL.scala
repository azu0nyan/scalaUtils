package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL.{DCELDataTypes, Face, HalfEdge, Vertex}
import utils.datastructures.dcel.HierarchicalDCELCache.HierarchicalDCELCacheInstance
import utils.datastructures.graph.VisibilityGraphOps
import utils.math.planar.{Polygon, PolygonRegion, SegmentPlanar, V2}
import utils.datastructures.dcel.PlanarDCEL.PlanarEdge
import utils.math._
import utils.sugar.{IteratorOps, SeqOps}

object HierarchicalDCEL {


  type HierarchicalDCELOwnData = {
    type VertexOwnData
    type HalfEdgeOwnData
    type FaceOwnData
  }

  type HierarchicalDCELData[OwnData <: HierarchicalDCELOwnData] = DCELDataTypes{
     type VertexData = HierarchicalVertex[OwnData]
     type HalfEdgeData = HierarchicalEdge[OwnData]
     type FaceData =  HierarchicalFace[OwnData]
  }

  type HierarchicalDCEL[OwnData <: HierarchicalDCELOwnData] = PlanarDCEL[HierarchicalDCELData[OwnData]]
  //R stands for RAW
  type RVertex[OwnData <: HierarchicalDCELOwnData] = DCEL.Vertex[HierarchicalDCELData[OwnData]]
  type RHalfEdge[OwnData <: HierarchicalDCELOwnData] = DCEL.HalfEdge[HierarchicalDCELData[OwnData]]
  type RFace[OwnData <: HierarchicalDCELOwnData] = DCEL.Face[HierarchicalDCELData[OwnData]]

//  type HierarchicalDCEL[VD, HED, FD] =
//    PlanarDCEL[HierarchicalVertex[VD, HED, FD], HierarchicalEdge[VD, HED, FD], HierarchicalFace[VD, HED, FD]]
//
//  type Face[VD, HED, FD] = Face[HierarchicalVertex[VD, HED, FD], HierarchicalEdge[VD, HED, FD], HierarchicalFace[VD, HED, FD]]
//  type HalfEdge[VD, HED, FD] = HalfEdge[HierarchicalVertex[VD, HED, FD], HierarchicalEdge[VD, HED, FD], HierarchicalFace[VD, HED, FD]]
//  type Vertex[VD, HED, FD] = Vertex[HierarchicalVertex[VD, HED, FD], HierarchicalEdge[VD, HED, FD], HierarchicalFace[VD, HED, FD]]


  class HierarchicalVertex[OD <: HierarchicalDCELOwnData](var ownData: OD#VertexOwnData)
                                                         (implicit extractor: OD#VertexOwnData => V2) {

    def position: V2 = extractor(ownData)
  }

  /**
    * @param isFake true if edge lies on border of some inner polygon, have non-fake ancestor
    */
  class HierarchicalEdge[OD <: HierarchicalDCELOwnData](
                                                         initialParents: Seq[HierarchicalEdge[OD]],
                                                         //var isFake: Boolean, //todo remove, if edge has parent than it's twin has partially fake part
                                                         var ownData: OD#HalfEdgeOwnData) {


    var edge: HalfEdge[HierarchicalDCELData[OD]] = _

    var childs: Seq[HierarchicalEdge[OD]] = Seq()

    private[this] var _parents: Seq[HierarchicalEdge[OD]] = Seq()

    initialParents.foreach(addParent)

    /** Parent edge can cover only part of a child, full cover not guaranteed.
      * Because modification of innerDCEL shouldn't affect modifications of a parent,
      * and splitting child edge at parent vertices can be impossible */
    def parents: Seq[HierarchicalEdge[OD]] = _parents

    def face: RFace[OD] = edge.leftFace

//    def hierarchicalFace: HierarchicalFace[OD] = edge.leftFace.data

    def asSegment: SegmentPlanar = SegmentPlanar(edge.origin.data.position, edge.ending.data.position)

    /** Parent edge can cover only part of a child, full cover not guaranteed */
    def allLevelChilds: Seq[HierarchicalEdge[OD]] = childs.flatMap(_.meAndAllLevelChilds.toSeq)

    /** Parent edge can cover only part of a child, full cover not guaranteed */
    def meAndAllLevelChilds: Seq[HierarchicalEdge[OD]] = this +: allLevelChilds

    //    def rootParent: HierarchicalEdge[OD] = parent.map(_.rootParent).getOrElse(this)
    //
    //    def meAndAncestors: Iterator[HierarchicalEdge[OD]] = Iterator.from(Seq(this)) ++ ancestors
    //
    //    def ancestors: Iterator[HierarchicalEdge[OD]] = IteratorOps.iterateOpt(parent)(_.parent)

    /** twin on same or higher level to whom we go if go through EdgeNode(this) */
    //    def edgeNodeTwin: HierarchicalEdge[OD] =
    //      if (!edge.twin.data.isFake) edge.twin.data
    //      else parent.map(_.edgeNodeTwin).get //in that case parent shouldn't be empty and root level parent always have non-fake twin
    //

    def addParent(toAdd: HierarchicalEdge[OD]): Unit = {
      _parents = _parents :+ toAdd
      toAdd.childs = toAdd.childs :+ toAdd
    }

    def removeParent(toRemove: HierarchicalEdge[OD]): Unit = {
      _parents = _parents.filter(_ != toRemove)
      toRemove.childs = toRemove.childs.filter(_ != toRemove)
    }

    def replaceParents(newParents: Seq[HierarchicalEdge[OD]]) : Unit = {
      parents.foreach(removeParent)
      newParents.foreach(addParent)
    }

    /***/
    def parentCoveredParts: Seq[(HierarchicalEdge[OD], Scalar, Scalar)] = {
      val seg = asSegment
      parents.flatMap{parent =>
        //assumes that parent goes in the same direction as a child => start < end
        val pSeg = parent.asSegment
        val start = clamp(seg.getFractionAt(pSeg.start), 0, 1)
        val end = clamp(seg.getFractionAt(pSeg.end), 0, 1)
        //check that child has non point intersection with parent //todo mb delete check
        Option.when(start ~< 1 && (end !~= start))((parent, start, end))
      }
    }

  }

  class HierarchicalFace[OD <: HierarchicalDCELOwnData](
                                       val parent: Option[HierarchicalFace[OD]],
                                       var ownData: OD#FaceOwnData)(implicit extractor: OD#VertexOwnData => V2) {


    var face: RFace[OD] = _

    var cacheEnabled: Boolean = false
    var shapeCache: HierarchicalDCELCacheInstance[OD] = _
    val innerDCEL: HierarchicalDCEL[OD] = new HierarchicalDCEL(this, d => d.position)

    innerDCEL.onNewFace.subscribe { face =>
      face.data.face = face
    }

    innerDCEL.onNewHalfEdge.subscribe { edge =>
      edge.data.edge = edge
    }
    //        innerDCEL.onEdgeSplit.subscribe { case (old, newNext) =>
    //          todo split portals
    //          newNext.data.childOf(old.data.parent)
    //          newNext.twin.data.childOf(old.twin.data.parent)
    //        }

    innerDCEL.onNewFace(innerDCEL.outerFace)

    /** Face's outer border. Build from face.borderEdges */
    def outerBorder: Seq[RHalfEdge[OD]] = if (cacheEnabled) shapeCache.outerBorder else HierarchicalDCELCache.outerBorder(this)

    def holesOwnContours: Seq[Seq[RHalfEdge[OD]]] = if (cacheEnabled) shapeCache.holesOwnContours else HierarchicalDCELCache.holesOwnContours(this)

    def fullBorder: Seq[RHalfEdge[OD]] = if (cacheEnabled) shapeCache.fullBorder else HierarchicalDCELCache.fullBorder(this)

    /** Holes parent equals to  my parent */
    def holeAreas: Seq[HierarchicalFace[OD]] = if (cacheEnabled) shapeCache.holeAreas else HierarchicalDCELCache.holeAreas(this)

    def neighbourFaces: Seq[HierarchicalFace[OD]] = if (cacheEnabled) shapeCache.neighbourFaces else HierarchicalDCELCache.neighbourFaces(this)

    /** Childs which inner parent  is me */
    def directChildFaces: Seq[HierarchicalFace[OD]] = if (cacheEnabled) shapeCache.directChildFaces else HierarchicalDCELCache.directChildFaces(this)

    def allChildFaces: Seq[HierarchicalFace[OD]] = if (cacheEnabled) shapeCache.allChildFaces else HierarchicalDCELCache.allChildFaces(this)

    def outerPoly: PolygonRegion = if (cacheEnabled) shapeCache.outerPoly else HierarchicalDCELCache.outerPoly(this)

    def holesPolys: Seq[PolygonRegion] = if (cacheEnabled) shapeCache.holesPolys else HierarchicalDCELCache.holesPolys(this)

    def polygon: Polygon = if (cacheEnabled) shapeCache.polygon else HierarchicalDCELCache.polygon(this)

    /** Area that belong only to this face, no child's or holes contained in it */
    def ownArea: Polygon = if (cacheEnabled) shapeCache.ownArea else HierarchicalDCELCache.ownArea(this)

    /** Segments that borders ownArea */
    def obstacles: Seq[SegmentPlanar] = if (cacheEnabled) shapeCache.obstacles else HierarchicalDCELCache.obstacles(this)

    //////////////////////////helper functions
    def isAncestorOf(a: HierarchicalFace[OD]): Boolean = allChildFaces.contains(a)

    /** faster than isAncestorOf */
    def isDescendantOf(a: HierarchicalFace[OD]): Boolean = parent.contains(a) || parent.nonEmpty && parent.get.isDescendantOf(a)

    def containsPoint(pos: V2): Boolean = outerPoly.contains(pos) && !holesPolys.exists(_.containsInside(pos))

    def meAndParents: Seq[HierarchicalFace[OD]] = this +: parent.map(_.meAndParents).getOrElse(Seq())

    /** points should be inside */
    def visible(from: V2, to: V2): Boolean = //(from ~= to) ||
      ownArea.contains(from) && ownArea.contains(to) && ownArea.contains((from + to) * .5d) && {
        !obstacles.exists(seg => VisibilityGraphOps.bodyIntersection(from, to, seg.start, seg.end))
      }

    def faceAt(pos: V2): HierarchicalFace[OD] = {
      //    println(s"${areaGameplayData.name} $holeAreas}")
      if (outerPoly.contains(pos)) {
        directChildFaces.find(_.outerPoly.contains(pos)) orElse
          holeAreas.find(_.outerPoly.contains(pos)) match {
          case Some(value) =>
            value.faceAt(pos)
          case None => this
        }
      } else if (parent.nonEmpty) {
        parent.get.faceAt(pos)
      } else {
        //        log.error(s"Can't find area at $pos") //todo api???
        this
      }
    }


    //todo clamp
//    def splitEdge(edge: HalfEdge, splitAt: HierarchicalVertex[OD], endingData: HED, endingTwinData: HED): HalfEdge = {
      //todo split parents
//      innerDCEL.split(edge, splitAt,
//        new HierarchicalEdge[OD](edge.data.parent, endingData),
//        new HierarchicalEdge[OD](edge.twin.data.parent, endingTwinData))
//      edge.next
//    }


    /** can cut poly in own area with result of single polygon island */
    def canCutAsSingleArea(poly: PolygonRegion): Boolean = ??? //todo mb remove

    def findParentForEdge(seg: SegmentPlanar): Seq[HierarchicalEdge[OD]] = {
      fullBorder.filter { parent =>
        val parentSeg = parent.data.asSegment
        seg.body.sameDirection(parentSeg.body) &&  seg.haveSegmentIntersection(parentSeg)
      }.map(_.data)
    }

    def newHierarchicalEdgeProvider(v1: RVertex[OD], v2: RVertex[OD], newHEDProvider: (RVertex[OD], RVertex[OD]) => (OD#HalfEdgeOwnData, OD#HalfEdgeOwnData)): (HierarchicalEdge[OD], HierarchicalEdge[OD]) = {
      val (data, twinData) = newHEDProvider(v1, v2)
      val seg = SegmentPlanar(v1.data.position, v2.data.position)
      val parents = findParentForEdge(seg)
      val twinParents = findParentForEdge(seg.reverse)

      (new HierarchicalEdge[OD](parents, data), new HierarchicalEdge[OD](twinParents, twinData))
    }

    def splitHierarchicalEdgeProvider(edge: RHalfEdge[OD], at: V2, splitHEDProvider: (RHalfEdge[OD], V2) => (OD#HalfEdgeOwnData, OD#HalfEdgeOwnData)): (HierarchicalEdge[OD], HierarchicalEdge[OD]) = {
      val (data, twinData) = splitHEDProvider(edge, at)
      val SegmentPlanar(start, end) = edge.data.asSegment
      //todo do int here or in linstener??
      val parents = findParentForEdge(SegmentPlanar(start, at))
      val twinParents = findParentForEdge(SegmentPlanar(at, start))
      edge.data.replaceParents(parents)
      edge.twin.data.replaceParents(twinParents)

      val newParents = findParentForEdge(SegmentPlanar(at, end))
      val newTwinParents = findParentForEdge(SegmentPlanar(end, at))

      (new HierarchicalEdge[OD](newParents,  data),
        new HierarchicalEdge[OD](newTwinParents, twinData))
    }

    def newHierarchicalFaceProvider(edge: RHalfEdge[OD], newFDProvider: RHalfEdge[OD] => OD#FaceOwnData): HierarchicalFace[OD] = {
      new HierarchicalFace[OD](Some(this), newFDProvider(edge))
    }

    //todo normalization needed?
    def cutChain(chain: Seq[V2],
                 newVDProvider: V2 => OD#VertexOwnData,
                 newHEDProvider: (RVertex[OD], RVertex[OD]) => (OD#HalfEdgeOwnData, OD#HalfEdgeOwnData),
                 splitHEDProvider: (RHalfEdge[OD], V2) => (OD#HalfEdgeOwnData, OD#HalfEdgeOwnData),
                 newFDProvider: RHalfEdge[OD] => OD#FaceOwnData): Seq[RHalfEdge[OD]] = {
      val normalized = SeqOps.removeConsecutiveDuplicatesCircular(chain.toList)
      val res = innerDCEL.cutChain(normalized,
        x => new HierarchicalVertex[OD](newVDProvider(x)),
        newHierarchicalEdgeProvider(_, _, newHEDProvider),
        splitHierarchicalEdgeProvider(_, _, splitHEDProvider),
        newHierarchicalFaceProvider(_, newFDProvider))
      DCELOps.toChain(res).flatten.toSeq
    }

    /** Cuts PolygonRegion inside face(in innerDCEL), polygonRegion should be clamped before cut. */
    def cutClamped(poly: PolygonRegion,
                   newVDProvider: V2 => OD#VertexOwnData,
                   newHEDProvider: (RVertex[OD], RVertex[OD]) => (OD#HalfEdgeOwnData, OD#HalfEdgeOwnData),
                   splitHEDProvider: (RHalfEdge[OD], V2) => (OD#HalfEdgeOwnData, OD#HalfEdgeOwnData),
                   newFDProvider: RHalfEdge[OD] => OD#FaceOwnData): Seq[RHalfEdge[OD]] = {
      val normalized = PolygonRegion(SeqOps.removeConsecutiveDuplicatesCircular(poly.vertices.toList))
      val p = if (normalized.isCw) normalized.reverse else normalized

      val res = innerDCEL.cutPoly(p.vertices,
        x => new HierarchicalVertex[OD](newVDProvider(x)),
        newHierarchicalEdgeProvider(_, _, newHEDProvider),
        splitHierarchicalEdgeProvider(_, _, splitHEDProvider),
        newHierarchicalFaceProvider(_, newFDProvider))
      DCELOps.toChain(res).flatten.toSeq
    }


    def enableCaches(): Unit = {
      cacheEnabled = false
      directChildFaces.foreach(_.enableCaches())
      updateCache()
      cacheEnabled = true
    }

    def updateCache(): Unit = {
      HierarchicalDCELCache.directChildFaces(this).foreach(_.updateCacheData())
      updateCacheData()
    }

    def updateCacheData(): Unit = {
      shapeCache = HierarchicalDCELCache(this)
    }

    def disableCaches(): Unit = {
      cacheEnabled = false
      directChildFaces.foreach(_.disableCaches())
    }


  }


}

