package utils.datastructures.dcel

import utils.datastructures.dcel.DCEL.{DCELData, Face, HalfEdge, Vertex}
import utils.datastructures.dcel.HierarchicalDCELCache.HierarchicalDCELCacheInstance
import utils.datastructures.graph.VisibilityGraphOps
import utils.math.planar.{Polygon, PolygonRegion, SegmentPlanar, V2}
import utils.datastructures.dcel.PlanarDCEL.PlanarEdge
import utils.math._
import utils.sugar.{IteratorOps, SeqOps}


object HierarchicalDCEL {

  //OD means own data

  type HierarchicalDCEL[VOD, HOD, FOD] =
    PlanarDCEL[HierarchicalVertex[VOD, HOD, FOD], HierarchicalEdge[VOD, HOD, FOD], HierarchicalFace[VOD, HOD, FOD]]
  //R stands for RAW
  //Helper types to reduce boilerplate
  type RVertex[VOD, HOD, FOD] =
    DCEL.Vertex[HierarchicalVertex[VOD, HOD, FOD], HierarchicalEdge[VOD, HOD, FOD], HierarchicalFace[VOD, HOD, FOD]]
  type RHalfEdge[VOD, HOD, FOD] =
    DCEL.HalfEdge[HierarchicalVertex[VOD, HOD, FOD], HierarchicalEdge[VOD, HOD, FOD], HierarchicalFace[VOD, HOD, FOD]]
  type RFace[VOD, HOD, FOD] =
    DCEL.Face[HierarchicalVertex[VOD, HOD, FOD], HierarchicalEdge[VOD, HOD, FOD], HierarchicalFace[VOD, HOD, FOD]]

  /** Collection of constructors for own data */
  trait OwnDataProvider[VOD, HOD, FOD] {
    def newFaceData(edge: RHalfEdge[VOD, HOD, FOD]): FOD

    def newVertexData(v: V2): VOD

    def newEdgeData(v1: RVertex[VOD, HOD, FOD], v2: RVertex[VOD, HOD, FOD]): (HOD, HOD)

    def splitEdgeData(edge: RHalfEdge[VOD, HOD, FOD], at: V2): (HOD, HOD)
  }

  /** Constructors for Hierarchical elements, takes constructors for own data as arguments
   */
  class HierarchicalDCElDataProvider[VOD, HOD, FOD](
                                                     val ownDataProvider: OwnDataProvider[VOD, HOD, FOD],
                                                     val setupFace: HierarchicalFace[VOD, HOD, FOD] => Unit = (x: HierarchicalFace[VOD, HOD, FOD]) => (),
                                                     val setupHalfEdge: HierarchicalEdge[VOD, HOD, FOD] => Unit = (x: HierarchicalEdge[VOD, HOD, FOD]) => (),
                                                     val setupVertex: HierarchicalVertex[VOD, HOD, FOD] => Unit = (x: HierarchicalVertex[VOD, HOD, FOD]) => (),
                                                   )
                                                   (implicit extractor: VOD => V2)
    extends DCELDataProvider[HierarchicalVertex[VOD, HOD, FOD], HierarchicalEdge[VOD, HOD, FOD], HierarchicalFace[VOD, HOD, FOD]] {
    var thisHierarchicalFace: HierarchicalFace[VOD, HOD, FOD] = _
    def setFace(f: HierarchicalFace[VOD, HOD, FOD]): Unit = thisHierarchicalFace = f


    override def newFaceData(edge: HalfEdge[HierarchicalVertex[VOD, HOD, FOD], HierarchicalEdge[VOD, HOD, FOD], HierarchicalFace[VOD, HOD, FOD]]): HierarchicalFace[VOD, HOD, FOD] = {
      val pr = new HierarchicalDCElDataProvider[VOD, HOD, FOD](ownDataProvider, setupFace, setupHalfEdge, setupVertex)
      val f = new HierarchicalFace[VOD, HOD, FOD](Some(thisHierarchicalFace), ownDataProvider.newFaceData(edge), pr)
      //no call of setupFace(f), because setupFace called in constructor of HierarchicalFace
      f
    }

    override def newVertexData(v: V2): HierarchicalVertex[VOD, HOD, FOD] = {
      val hv = new HierarchicalVertex[VOD, HOD, FOD](ownDataProvider.newVertexData(v))
      setupVertex(hv)
      hv
    }

    override def newEdgeData(v1: RVertex[VOD, HOD, FOD], v2: RVertex[VOD, HOD, FOD]): (HierarchicalEdge[VOD, HOD, FOD], HierarchicalEdge[VOD, HOD, FOD]) = {
      val (data, twinData) = ownDataProvider.newEdgeData(v1, v2)
      val seg = SegmentPlanar(v1.data.position, v2.data.position)
      val parents = thisHierarchicalFace.findParentForEdge(seg)
      val twinParents = thisHierarchicalFace.findParentForEdge(seg.reverse)

      val (he, the) = (new HierarchicalEdge[VOD, HOD, FOD](parents, data), new HierarchicalEdge[VOD, HOD, FOD](twinParents, twinData))
      setupHalfEdge(he)
      setupHalfEdge(the)
      (he, the)
    }

    override def splitEdgeData(edge: RHalfEdge[VOD, HOD, FOD], at: V2): (HierarchicalEdge[VOD, HOD, FOD], HierarchicalEdge[VOD, HOD, FOD]) = {
      val (data, twinData) = ownDataProvider.splitEdgeData(edge, at)
      val SegmentPlanar(start, end) = edge.data.asSegment
      //todo do int here or in linstener??
      val parents = thisHierarchicalFace.findParentForEdge(SegmentPlanar(start, at))
      val twinParents = thisHierarchicalFace.findParentForEdge(SegmentPlanar(at, start))
      edge.data.replaceParents(parents)
      edge.twin.data.replaceParents(twinParents)

      val newParents = thisHierarchicalFace.findParentForEdge(SegmentPlanar(at, end))
      val newTwinParents = thisHierarchicalFace.findParentForEdge(SegmentPlanar(end, at))

      val (he, the) = (new HierarchicalEdge[VOD, HOD, FOD](newParents, data),
        new HierarchicalEdge[VOD, HOD, FOD](newTwinParents, twinData))

      setupHalfEdge(he)
      setupHalfEdge(the)
      (he, the)
    }
  }


  class HierarchicalVertex[VOD, HOD, FOD](var ownData: VOD)
                                         (implicit extractor: VOD => V2) {

    def position: V2 = extractor(ownData)
  }

  /**
   * @param isFake true if edge lies on border of some inner polygon, have non-fake ancestor
   */
  class HierarchicalEdge[VOD, HOD, FOD](
                                         initialParents: Seq[HierarchicalEdge[VOD, HOD, FOD]],
                                         //var isFake: Boolean, //todo remove, if edge has parent than it's twin has partially fake part
                                         var ownData: HOD) {

    var edge: HalfEdge[HierarchicalVertex[VOD, HOD, FOD], HierarchicalEdge[VOD, HOD, FOD], HierarchicalFace[VOD, HOD, FOD]] = _

    var childs: Seq[HierarchicalEdge[VOD, HOD, FOD]] = Seq()

    private[this] var _parents: Seq[HierarchicalEdge[VOD, HOD, FOD]] = Seq()

    initialParents.foreach(addParent)

    /** Parent edge can cover only part of a child, full cover not guaranteed.
     * Because modification of innerDCEL shouldn't affect modifications of a parent,
     * and splitting child edge at parent vertices can be impossible */
    def parents: Seq[HierarchicalEdge[VOD, HOD, FOD]] = _parents


    /** isEdgeThat's leftFace is outer face todo check isEmpty */
    def isOuterEdge: Boolean = edge.leftFace.data.parent.isEmpty || edge.leftFace.data.parent.contains(edge.leftFace.data)

    def face: RFace[VOD, HOD, FOD] = edge.leftFace

    //    def hierarchicalFace: HierarchicalFace[VOD, HOD, FOD] = edge.leftFace.data

    @inline def asSegment: SegmentPlanar = SegmentPlanar(edge.origin.data.position, edge.ending.data.position)

    def childsMeAndParents: Seq[HierarchicalEdge[VOD, HOD, FOD]] = (allLevelChilds :+ this) ++ allLevelParents

    def allLevelParents: Seq[HierarchicalEdge[VOD, HOD, FOD]] = parents.flatMap(p => p +: p.allLevelParents)

    /** Parent edge can cover only part of a child, full cover not guaranteed */
    def allLevelChilds: Seq[HierarchicalEdge[VOD, HOD, FOD]] = childs.flatMap(_.meAndAllLevelChilds.toSeq)

    /** Parent edge can cover only part of a child, full cover not guaranteed */
    def meAndAllLevelChilds: Seq[HierarchicalEdge[VOD, HOD, FOD]] = this +: allLevelChilds

    //    def rootParent: HierarchicalEdge[VOD, HOD, FOD] = parent.map(_.rootParent).getOrElse(this)
    //
    //    def meAndAncestors: Iterator[HierarchicalEdge[VOD, HOD, FOD]] = Iterator.from(Seq(this)) ++ ancestors
    //
    //    def ancestors: Iterator[HierarchicalEdge[VOD, HOD, FOD]] = IteratorOps.iterateOpt(parent)(_.parent)

    /** twin on same or higher level to whom we go if go through EdgeNode(this) */
    //    def edgeNodeTwin: HierarchicalEdge[VOD, HOD, FOD] =
    //      if (!edge.twin.data.isFake) edge.twin.data
    //      else parent.map(_.edgeNodeTwin).get //in that case parent shouldn't be empty and root level parent always have non-fake twin
    //

    def addParent(toAdd: HierarchicalEdge[VOD, HOD, FOD]): Unit = {
      _parents = _parents :+ toAdd
      toAdd.childs = toAdd.childs :+ toAdd
    }

    def removeParent(toRemove: HierarchicalEdge[VOD, HOD, FOD]): Unit = {
      _parents = _parents.filter(_ != toRemove)
      toRemove.childs = toRemove.childs.filter(_ != toRemove)
    }

    def replaceParents(newParents: Seq[HierarchicalEdge[VOD, HOD, FOD]]): Unit = {
      parents.foreach(removeParent)
      newParents.foreach(addParent)
    }

    /** Fraction intervals of parent halfEdges, that intersects with this half Edge */
    def parentCoveredIntervals: Seq[(HierarchicalEdge[VOD, HOD, FOD], Scalar, Scalar)] = {
      val seg = asSegment
      parents.flatMap { parent =>
        //assumes that parent goes in the same direction as a child => start < end
        val pSeg = parent.asSegment
        val start = clamp(seg.getFractionAt(pSeg.start), 0, 1)
        val end = clamp(seg.getFractionAt(pSeg.end), 0, 1)
        //check that child has non point intersection with parent //todo mb delete check
        Option.when(start ~< 1 && (end !~= start))((parent, start, end))
      }
    }

    /** Fraction intervals of direct child halfEdges, that intersects with this half Edge */
    def childCoveredIntervals: Seq[(HierarchicalEdge[VOD, HOD, FOD], Scalar, Scalar)] = {
      val seg = asSegment
      childs.flatMap { child =>
        //assumes that child goes in the same direction
        val cSeg = child.asSegment
        val start = clamp(seg.getFractionAt(cSeg.start), 0, 1)
        val end = clamp(seg.getFractionAt(cSeg.end), 0, 1)
        Option.when(start ~< 1 && (end !~= start))((child, start, end)) //todo mb delete check
      }
    }

    /** Fraction intervals that didn't covered by any child */
    def childUncoveredIntervals: Seq[(Scalar, Scalar)] = {
      val childsStartsEnds: Seq[Scalar] = childCoveredIntervals.sortBy(_._2).flatMap { case (_, start, end) => Seq(start, end) }
      val points: Seq[Scalar] = 0d +: (childsStartsEnds :+ 1d)
      points.sliding(2, 2).zipWithIndex.filter(_._2 % 2 == 0).flatMap {
        case (Seq(a, b), _) if a !~= b => Some((a, b))
        case _ => None
      }.toSeq
    }


  }


  class HierarchicalFace[VOD, HOD, FOD](
                                         val parent: Option[HierarchicalFace[VOD, HOD, FOD]],
                                         var ownData: FOD,
                                         dataProvider: HierarchicalDCElDataProvider[VOD, HOD, FOD]
                                       )(implicit extractor: VOD => V2) {
    thisHierarchicalFace =>

    dataProvider.setFace(thisHierarchicalFace)
    dataProvider.setupFace(this)

    var face: RFace[VOD, HOD, FOD] = _

    var cacheEnabled: Boolean = false
    var shapeCache: HierarchicalDCELCacheInstance[VOD, HOD, FOD] = _
    val innerDCEL: HierarchicalDCEL[VOD, HOD, FOD] = new HierarchicalDCEL(this, d => d.position)

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
    def outerBorder: Seq[RHalfEdge[VOD, HOD, FOD]] = if (cacheEnabled) shapeCache.outerBorder else HierarchicalDCELCache.outerBorder(this)

    def holesOwnContours: Seq[Seq[RHalfEdge[VOD, HOD, FOD]]] = if (cacheEnabled) shapeCache.holesOwnContours else HierarchicalDCELCache.holesOwnContours(this)

    def fullBorder: Seq[RHalfEdge[VOD, HOD, FOD]] = if (cacheEnabled) shapeCache.fullBorder else HierarchicalDCELCache.fullBorder(this)

    /** Holes parent equals to  my parent */
    def holeAreas: Seq[HierarchicalFace[VOD, HOD, FOD]] = if (cacheEnabled) shapeCache.holeAreas else HierarchicalDCELCache.holeAreas(this)

    def neighbourFaces: Seq[HierarchicalFace[VOD, HOD, FOD]] = if (cacheEnabled) shapeCache.neighbourFaces else HierarchicalDCELCache.neighbourFaces(this)

    /** Childs which inner parent  is me */
    def directChildFaces: Seq[HierarchicalFace[VOD, HOD, FOD]] = if (cacheEnabled) shapeCache.directChildFaces else HierarchicalDCELCache.directChildFaces(this)

    def allChildFaces: Seq[HierarchicalFace[VOD, HOD, FOD]] = if (cacheEnabled) shapeCache.allChildFaces else HierarchicalDCELCache.allChildFaces(this)

    def outerPoly: PolygonRegion = if (cacheEnabled) shapeCache.outerPoly else HierarchicalDCELCache.outerPoly(this)

    def holesPolys: Seq[PolygonRegion] = if (cacheEnabled) shapeCache.holesPolys else HierarchicalDCELCache.holesPolys(this)

    def polygon: Polygon = if (cacheEnabled) shapeCache.polygon else HierarchicalDCELCache.polygon(this)

    /** Area that belong only to this face, no child's or holes contained in it */
    def ownArea: Polygon = if (cacheEnabled) shapeCache.ownArea else HierarchicalDCELCache.ownArea(this)

    /** Segments that borders ownArea */
    def obstacles: Seq[SegmentPlanar] = if (cacheEnabled) shapeCache.obstacles else HierarchicalDCELCache.obstacles(this)

    //////////////////////////helper functions
    def isAncestorOf(a: HierarchicalFace[VOD, HOD, FOD]): Boolean = allChildFaces.contains(a)

    /** faster than isAncestorOf */
    def isDescendantOf(a: HierarchicalFace[VOD, HOD, FOD]): Boolean = parent.contains(a) || parent.nonEmpty && parent.get.isDescendantOf(a)

    def containsPoint(pos: V2): Boolean = outerPoly.contains(pos) && !holesPolys.exists(_.containsInside(pos))

    def meAndParents: Seq[HierarchicalFace[VOD, HOD, FOD]] = this +: parent.map(_.meAndParents).getOrElse(Seq())

    /** points should be inside */
    def visible(from: V2, to: V2): Boolean = //(from ~= to) ||
      ownArea.contains(from) && ownArea.contains(to) && ownArea.contains((from + to) * .5d) && {
        !obstacles.exists(seg => VisibilityGraphOps.bodyIntersection(from, to, seg.start, seg.end))
      }

    def faceAt(pos: V2): HierarchicalFace[VOD, HOD, FOD] = {
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
    //    def splitEdge(edge: HalfEdge, splitAt: HierarchicalVertex[VOD, HOD, FOD], endingData: HED, endingTwinData: HED): HalfEdge = {
    //todo split parents
    //      innerDCEL.split(edge, splitAt,
    //        new HierarchicalEdge[VOD, HOD, FOD](edge.data.parent, endingData),
    //        new HierarchicalEdge[VOD, HOD, FOD](edge.twin.data.parent, endingTwinData))
    //      edge.next
    //    }


    /** can cut poly in own area with result of single polygon island */
    def canCutAsSingleArea(poly: PolygonRegion): Boolean = ??? //todo mb remove

    def findParentForEdge(seg: SegmentPlanar): Seq[HierarchicalEdge[VOD, HOD, FOD]] = {
      fullBorder.filter { parent =>
        val parentSeg = parent.data.asSegment
        seg.body.sameDirection(parentSeg.body) && seg.haveSegmentIntersection(parentSeg)
      }.map(_.data)
    }


    //todo normalization needed?
    def cutChain(chain: Seq[V2]): Seq[RHalfEdge[VOD, HOD, FOD]] = {
      val normalized = SeqOps.removeConsecutiveDuplicatesCircular(chain.toList)
      val res = innerDCEL.cutChain(normalized, dataProvider)
      DCELOps.toChainOpt(res).flatten.toSeq
    }

    /** Cuts PolygonRegion inside face(in innerDCEL), polygonRegion should be clamped before cut. */
    def cutClamped(poly: PolygonRegion): Seq[RHalfEdge[VOD, HOD, FOD]] = {
      val normalized = PolygonRegion(SeqOps.removeConsecutiveDuplicatesCircular(poly.vertices.toList))
      val p = if (normalized.isCw) normalized.reverse else normalized

      val res = innerDCEL.cutPoly(p.vertices, dataProvider)
      DCELOps.toChainOpt(res).flatten.toSeq
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

