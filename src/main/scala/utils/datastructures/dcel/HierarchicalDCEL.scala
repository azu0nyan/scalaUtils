package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL.{RawFace, RawHalfEdge, RawVertex}
import utils.datastructures.dcel.HierarchicalDCELCache.HierarchicalDCELCacheInstance
import utils.datastructures.graph.VisibilityGraphOps
import utils.math.planar.{Polygon, PolygonRegion, SegmentPlanar, V2}
import utils.datastructures.dcel.PlanarDCEL.PlanarEdge
import utils.math._
import utils.sugar.{IteratorOps, SeqOps}

object HierarchicalDCEL {

  type HierarchicalDCEL[VD, HED, FD] =
    PlanarDCEL[HierarchicalVertex[VD, HED, FD], HierarchicalEdge[VD, HED, FD], HierarchicalFace[VD, HED, FD]]

  type Face[VD, HED, FD] = RawFace[HierarchicalVertex[VD, HED, FD], HierarchicalEdge[VD, HED, FD], HierarchicalFace[VD, HED, FD]]
  type HalfEdge[VD, HED, FD] = RawHalfEdge[HierarchicalVertex[VD, HED, FD], HierarchicalEdge[VD, HED, FD], HierarchicalFace[VD, HED, FD]]
  type Vertex[VD, HED, FD] = RawVertex[HierarchicalVertex[VD, HED, FD], HierarchicalEdge[VD, HED, FD], HierarchicalFace[VD, HED, FD]]


  class HierarchicalVertex[VD, HED, FD](var ownData: VD)(implicit extractor: VD => V2) {
    type Face = HierarchicalDCEL.Face[VD, HED, FD]
    type HalfEdge = HierarchicalDCEL.HalfEdge[VD, HED, FD]
    type Vertex = HierarchicalDCEL.Vertex[VD, HED, FD]

    def position: V2 = extractor(ownData)
  }

  /**
    * @param isFake true if edge lies on border of some inner polygon, have non-fake ancestor
    */
  class HierarchicalEdge[VD, HED, FD](
                                       initialParents: Seq[HierarchicalEdge[VD, HED, FD]],
                                       //var isFake: Boolean, //todo remove, if edge has parent than it's twin has partially fake part
                                       var ownData: HED) {
    type Face = HierarchicalDCEL.Face[VD, HED, FD]
    type HalfEdge = HierarchicalDCEL.HalfEdge[VD, HED, FD]
    type Vertex = HierarchicalDCEL.Vertex[VD, HED, FD]


    var edge: HalfEdge = _

    var childs: Seq[HierarchicalEdge[VD, HED, FD]] = Seq()

    private[this] var _parents: Seq[HierarchicalEdge[VD, HED, FD]] = Seq()

    initialParents.foreach(addParent)

    /** Parent edge can cover only part of a child, full cover not guaranteed.
      * Because modification of innerDCEL shouldn't affect modifications of a parent,
      * and splitting child edge at parent vertices can be impossible */
    def parents: Seq[HierarchicalEdge[VD, HED, FD]] = _parents

    def face: Face = edge.leftFace

    def hierarchicalFace: HierarchicalFace[VD, HED, FD] = edge.leftFace.data

    def asSegment: SegmentPlanar = SegmentPlanar(edge.origin.data.position, edge.ending.data.position)

    /** Parent edge can cover only part of a child, full cover not guaranteed */
    def allLevelChilds: Seq[HierarchicalEdge[VD, HED, FD]] = childs.flatMap(_.meAndAllLevelChilds.toSeq)

    /** Parent edge can cover only part of a child, full cover not guaranteed */
    def meAndAllLevelChilds: Seq[HierarchicalEdge[VD, HED, FD]] = this +: allLevelChilds

    //    def rootParent: HierarchicalEdge[VD, HED, FD] = parent.map(_.rootParent).getOrElse(this)
    //
    //    def meAndAncestors: Iterator[HierarchicalEdge[VD, HED, FD]] = Iterator.from(Seq(this)) ++ ancestors
    //
    //    def ancestors: Iterator[HierarchicalEdge[VD, HED, FD]] = IteratorOps.iterateOpt(parent)(_.parent)

    /** twin on same or higher level to whom we go if go through EdgeNode(this) */
    //    def edgeNodeTwin: HierarchicalEdge[VD, HED, FD] =
    //      if (!edge.twin.data.isFake) edge.twin.data
    //      else parent.map(_.edgeNodeTwin).get //in that case parent shouldn't be empty and root level parent always have non-fake twin
    //

    def addParent(toAdd: HierarchicalEdge[VD, HED, FD]): Unit = {
      _parents = _parents :+ toAdd
      toAdd.childs = toAdd.childs :+ toAdd
    }

    def removeParent(toRemove: HierarchicalEdge[VD, HED, FD]): Unit = {
      _parents = _parents.filter(_ != toRemove)
      toRemove.childs = toRemove.childs.filter(_ != toRemove)
    }

    def replaceParents(newParents: Seq[HierarchicalEdge[VD, HED, FD]]) : Unit = {
      parents.foreach(removeParent)
      newParents.foreach(addParent)
    }

    /***/
    def parentCoveredParts: Seq[(HierarchicalEdge[VD, HED, FD], Scalar, Scalar)] = {
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

  class HierarchicalFace[VD, HED, FD](
                                       val parent: Option[HierarchicalFace[VD, HED, FD]],
                                       var ownData: FD)(implicit extractor: VD => V2) {
    type Face = HierarchicalDCEL.Face[VD, HED, FD]
    type HalfEdge = HierarchicalDCEL.HalfEdge[VD, HED, FD]
    type Vertex = HierarchicalDCEL.Vertex[VD, HED, FD]


    var face: Face = _

    var cacheEnabled: Boolean = false
    var shapeCache: HierarchicalDCELCacheInstance[VD, HED, FD] = _
    val innerDCEL: HierarchicalDCEL[VD, HED, FD] = new HierarchicalDCEL(this, d => d.position)

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
    def outerBorder: Seq[HalfEdge] = if (cacheEnabled) shapeCache.outerBorder else HierarchicalDCELCache.outerBorder(this)

    def holesOwnContours: Seq[Seq[HalfEdge]] = if (cacheEnabled) shapeCache.holesOwnContours else HierarchicalDCELCache.holesOwnContours(this)

    def fullBorder: Seq[HalfEdge] = if (cacheEnabled) shapeCache.fullBorder else HierarchicalDCELCache.fullBorder(this)

    /** Holes parent equals to  my parent */
    def holeAreas: Seq[HierarchicalFace[VD, HED, FD]] = if (cacheEnabled) shapeCache.holeAreas else HierarchicalDCELCache.holeAreas(this)

    def neighbourFaces: Seq[HierarchicalFace[VD, HED, FD]] = if (cacheEnabled) shapeCache.neighbourFaces else HierarchicalDCELCache.neighbourFaces(this)

    /** Childs which inner parent  is me */
    def directChildFaces: Seq[HierarchicalFace[VD, HED, FD]] = if (cacheEnabled) shapeCache.directChildFaces else HierarchicalDCELCache.directChildFaces(this)

    def allChildFaces: Seq[HierarchicalFace[VD, HED, FD]] = if (cacheEnabled) shapeCache.allChildFaces else HierarchicalDCELCache.allChildFaces(this)

    def outerPoly: PolygonRegion = if (cacheEnabled) shapeCache.outerPoly else HierarchicalDCELCache.outerPoly(this)

    def holesPolys: Seq[PolygonRegion] = if (cacheEnabled) shapeCache.holesPolys else HierarchicalDCELCache.holesPolys(this)

    def polygon: Polygon = if (cacheEnabled) shapeCache.polygon else HierarchicalDCELCache.polygon(this)

    /** Area that belong only to this face, no child's or holes contained in it */
    def ownArea: Polygon = if (cacheEnabled) shapeCache.ownArea else HierarchicalDCELCache.ownArea(this)

    /** Segments that borders ownArea */
    def obstacles: Seq[SegmentPlanar] = if (cacheEnabled) shapeCache.obstacles else HierarchicalDCELCache.obstacles(this)

    //////////////////////////helper functions
    def isAncestorOf(a: HierarchicalFace[VD, HED, FD]): Boolean = allChildFaces.contains(a)

    /** faster than isAncestorOf */
    def isDescendantOf(a: HierarchicalFace[VD, HED, FD]): Boolean = parent.contains(a) || parent.nonEmpty && parent.get.isDescendantOf(a)

    def containsPoint(pos: V2): Boolean = outerPoly.contains(pos) && !holesPolys.exists(_.containsInside(pos))

    def meAndParents: Seq[HierarchicalFace[VD, HED, FD]] = this +: parent.map(_.meAndParents).getOrElse(Seq())

    /** points should be inside */
    def visible(from: V2, to: V2): Boolean = //(from ~= to) ||
      ownArea.contains(from) && ownArea.contains(to) && ownArea.contains((from + to) * .5d) && {
        !obstacles.exists(seg => VisibilityGraphOps.bodyIntersection(from, to, seg.start, seg.end))
      }

    def faceAt(pos: V2): HierarchicalFace[VD, HED, FD] = {
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
//    def splitEdge(edge: HalfEdge, splitAt: HierarchicalVertex[VD, HED, FD], endingData: HED, endingTwinData: HED): HalfEdge = {
      //todo split parents
//      innerDCEL.split(edge, splitAt,
//        new HierarchicalEdge[VD, HED, FD](edge.data.parent, endingData),
//        new HierarchicalEdge[VD, HED, FD](edge.twin.data.parent, endingTwinData))
//      edge.next
//    }


    /** can cut poly in own area with result of single polygon island */
    def canCutAsSingleArea(poly: PolygonRegion): Boolean = ??? //todo mb remove

    def findParentForEdge(seg: SegmentPlanar): Seq[HierarchicalEdge[VD, HED, FD]] = {
      fullBorder.filter { parent =>
        val parentSeg = parent.data.asSegment
        seg.body.sameDirection(parentSeg.body) && {
          val start = clamp(seg.getFractionAt(parentSeg.start), 0, 1)
          val end = clamp(seg.getFractionAt(parentSeg.end), 0, 1)
          (start ~< 1 && (end !~= start))
        }
      }.map(_.data)
    }

    def newHierarchicalEdgeProvider(v1: Vertex, v2: Vertex, newHEDProvider: (Vertex, Vertex) => (HED, HED)): (HierarchicalEdge[VD, HED, FD], HierarchicalEdge[VD, HED, FD]) = {
      val (data, twinData) = newHEDProvider(v1, v2)
      val seg = SegmentPlanar(v1.data.position, v2.data.position)
      val parents = findParentForEdge(seg)
      val twinParents = findParentForEdge(seg.reverse)

      (new HierarchicalEdge[VD, HED, FD](parents, data), new HierarchicalEdge[VD, HED, FD](twinParents, twinData))
    }

    def splitHierarchicalEdgeProvider(edge: HalfEdge, at: V2, splitHEDProvider: (HalfEdge, V2) => (HED, HED)): (HierarchicalEdge[VD, HED, FD], HierarchicalEdge[VD, HED, FD]) = {
      val (data, twinData) = splitHEDProvider(edge, at)
      val SegmentPlanar(start, end) = edge.data.asSegment
      //todo do int here or in linstener??
      val parents = findParentForEdge(SegmentPlanar(start, at))
      val twinParents = findParentForEdge(SegmentPlanar(at, start))
      edge.data.replaceParents(parents)
      edge.twin.data.replaceParents(twinParents)

      val newParents = findParentForEdge(SegmentPlanar(at, end))
      val newTwinParents = findParentForEdge(SegmentPlanar(end, at))

      (new HierarchicalEdge[VD, HED, FD](newParents,  data),
        new HierarchicalEdge[VD, HED, FD](newTwinParents, twinData))
    }

    def newHierarchicalFaceProvider(edge: HalfEdge, newFDProvider: HalfEdge => FD): HierarchicalFace[VD, HED, FD] = {
      new HierarchicalFace[VD, HED, FD](Some(this), newFDProvider(edge))
    }

    //todo normalization needed?
    def cutChain(chain: Seq[V2],
                 newVDProvider: V2 => VD,
                 newHEDProvider: (Vertex, Vertex) => (HED, HED),
                 splitHEDProvider: (HalfEdge, V2) => (HED, HED),
                 newFDProvider: HalfEdge => FD): Seq[HalfEdge] = {
      val normalized = SeqOps.removeConsecutiveDuplicatesCircular(chain.toList)
      val res = innerDCEL.cutChain(normalized,
        x => new HierarchicalVertex[VD, HED, FD](newVDProvider(x)),
        newHierarchicalEdgeProvider(_, _, newHEDProvider),
        splitHierarchicalEdgeProvider(_, _, splitHEDProvider),
        newHierarchicalFaceProvider(_, newFDProvider))
      DCELOps.toChain(res).flatten.toSeq
    }

    /** Cuts PolygonRegion, polygonRegion should be clamped before cut. */
    def cutClamped(poly: PolygonRegion,
                   newVDProvider: V2 => VD,
                   newHEDProvider: (Vertex, Vertex) => (HED, HED),
                   splitHEDProvider: (HalfEdge, V2) => (HED, HED),
                   newFDProvider: HalfEdge => FD): Seq[HalfEdge] = {
      val normalized = PolygonRegion(SeqOps.removeConsecutiveDuplicatesCircular(poly.vertices.toList))
      val p = if (normalized.isCw) normalized.reverse else normalized

      val res = innerDCEL.cutPoly(p.vertices,
        x => new HierarchicalVertex[VD, HED, FD](newVDProvider(x)),
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

