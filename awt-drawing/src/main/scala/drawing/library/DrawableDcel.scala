package drawing.library

import drawing.core.SimpleDrawable
import drawing.library.ColorOps._
import drawing.library.ColorOps
import utils.datastructures.dcel.DCEL.RawHalfEdge
import utils.datastructures.dcel.PlanarDCEL
import utils.datastructures.dcel.PlanarDCEL
import utils.datastructures.dcel.PlanarDCEL._
import utils.math.{HALF, Scalar}
import utils.math.planar.{AngleOps, PolygonRegion, V2}

import java.awt.{Color, Graphics2D}

class DrawableDcel[VD <: V2, HED, FD](
                                       var dcel: PlanarDCEL[VD, HED, FD],
                                       var verticesColor: VD => Color = (x: VD) => new Color(0, 0, 255),
                                       var edgesColor: HED => Color = (x: HED) => new Color(10, 159, 10),
                                       var faceColor: FD => Color = (x: FD) => {
                                         import drawing.library.ColorOps._
                                         import drawing.library.ColorOps.ColorOps
                                         randomColor(x.##).setAlpha(30)
                                       },
                                       var angleOffset: Scalar = 15d,
                                       var drawHeData: Boolean = true,
                                       var drawHeConnections: Boolean = true,
                                       var drawHeToPolyLinks: Boolean = true,
                                       var drawVertexConnections: Boolean = true,
                                       var drawPolyBorders: Boolean = true,
                                       var drawHalfEdges:Boolean = true,
                                       var polyBorderColor: Color = new Color(30, 30, 30),
                                       var heTwinColor: Color = new Color(230, 30, 30),
                                       var hePrevColor: Color = new Color(30, 30, 230),
                                       var heNextPrevColor: Color = new Color(235, 30, 30),
                                       var heFaceColor: Color = new Color(30, 230, 30),
                                       val heToPolyColor: Color = new Color(240, 230, 50),
                                       val polyToHeColor: Color = new Color(80, 200, 200),
                                       val polyToHoleColor: Color = new Color(142, 50, 230),
                                       val vertexConnectionColors: Color = new Color(20, 30, 150)


                                     ) extends SimpleDrawable {
  override def drawAndUpdate(g: Graphics2D, dt: Scalar): Unit = {

    dcel.vertices.foreach { v =>
      val c = verticesColor(v.data)
      DrawingUtils.drawCircle(dcel.pos(v), angleOffset.toFloat / 4f, g, c, true)

      if (drawVertexConnections && v.incidentEdge.nonEmpty) {
        val (io, ie) = fromTo(v.incidentEdge.get)
        val to = utils.math.v2Lerp(io, ie, 0.1)
        DrawingUtils.drawArrow(dcel.pos(v), to, g, vertexConnectionColors, 2, 10)
      }
    }

    dcel.innerFaces.foreach { f =>
      val c = faceColor(f.data)
      val vs = f.outsideVertices
      DrawingUtils.drawPolygon(PolygonRegion(vs.map(v => dcel.extractor(v.data)).toSeq), g, true, c)
//      if (drawPolyBorders) {
//        DrawingUtils.drawPolygon(PolygonRegion(vs.map(v => dcel.extractor(v.data)).toSeq), g, false, polyBorderColor, 3)
//      }
      if (drawHeToPolyLinks && f.incidentEdge.nonEmpty) {
        val (origin, ending) = fromTo(f.incidentEdge.get)
        val onSeg = utils.math.v2Lerp(origin, ending, 0.3)
        val c = dcel.outerContour(f).center
        if (dcel.outerContour(f).contains(c)) {
          DrawingUtils.drawArrow(c, onSeg, g, polyToHeColor, 3, 20)
        } else {
          val dir = (ending - origin).rotate90CCW.normalize
          DrawingUtils.drawArrow(onSeg + dir * 30, onSeg, g, polyToHeColor, 3, 20)
        }
      }
      if(drawHeToPolyLinks){
        for(h <- f.holes){
          val (origin, ending) = fromTo(h)
          val onSeg = utils.math.v2Lerp(origin, ending, 0.4)

          val dir = (ending - origin).rotate90CCW.normalize
          DrawingUtils.drawArrow(onSeg + dir * 30, onSeg, g, polyToHoleColor, 3, 20)
          DrawingUtils.drawText(if(h.leftFace == dcel.outerFace)"outer" else h.leftFace.data.toString, onSeg + dir * 50, g,20)
        }
      }
    }
    if (drawPolyBorders) {
      dcel.innerFaces.foreach { f =>
        val vs = f.outsideVertices

        DrawingUtils.drawPolygon(PolygonRegion(vs.map(v => dcel.extractor(v.data)).toSeq), g, false, polyBorderColor, 2)
      }
    }


    if (drawHeToPolyLinks && dcel.outerFace.incidentEdge.nonEmpty) {
      val f = dcel.outerFace
      val (origin, ending) = fromTo(f.incidentEdge.get)
      val onSeg = utils.math.v2Lerp(origin, ending, 0.3)
      val dir = (ending - origin).rotate90CCW.normalize


      DrawingUtils.drawArrow(onSeg + dir * 30, onSeg, g, polyToHeColor, 4, 30)
    }


    if(drawHeToPolyLinks) {
      for (h <- dcel.outerFace.holes) {
        val (origin, ending) = fromTo(h)
        val onSeg = utils.math.v2Lerp(origin, ending, 0.4)

        val dir = (ending - origin).rotate90CCW.normalize
        DrawingUtils.drawArrow(onSeg + dir * 30, onSeg, g, polyToHoleColor, 3, 20)
        DrawingUtils.drawText(if(h.leftFace == dcel.outerFace)"outer" else h.leftFace.data.toString, onSeg + dir * 50, g,20)
      }
    }

    def fromTo(he: RawHalfEdge[VD, HED, FD]): (V2, V2) = {

      val p = dcel.pos(he.prev.origin)
      val c = dcel.pos(he.origin)
      val n = dcel.pos(he.next.origin)
      val nn = dcel.pos(he.next.ending)
      val offsetVector = AngleOps.ccwBisectorPath(p, c, n) * angleOffset
      val offsetVector2 = AngleOps.ccwBisectorPath(c, n, nn) * angleOffset

      val origin = he.origin.data + offsetVector
      val ending = he.ending.data + offsetVector2
      (origin, ending)
    }

    if (drawHalfEdges) {
      dcel.halfEdges.foreach { he =>
        val color = edgesColor(he.data)
        val (origin, ending) = fromTo(he)
        DrawingUtils.drawArrow(origin, ending, g, arrowHeadSize = 20.0, color = color, lineWidth = 2)


        if (drawHeConnections) {
          val middle = (origin + ending) * HALF
          val (to, te) = fromTo(he.twin)
          val tMiddle = (to + te) * HALF

          val (prevOrigin, _) = fromTo(he.prev)
          val (_, nextEnding) = fromTo(he.next)
          val toPrevStart = utils.math.v2Lerp(origin, ending, 0.05)
          val toPrevEnd = utils.math.v2Lerp(prevOrigin, origin, 0.95)

          val toNextStart = utils.math.v2Lerp(origin, ending, 0.9)
          val toNextEnd = utils.math.v2Lerp(ending, nextEnding, 0.1)

          DrawingUtils.drawArrow(toPrevStart, toPrevEnd, g, arrowHeadSize = 10.0, color = hePrevColor, lineWidth = 1)
          DrawingUtils.drawArrow(toNextStart, toNextEnd, g, arrowHeadSize = 10.0, color = heNextPrevColor, lineWidth = 1)
          DrawingUtils.drawArrow(middle, tMiddle, g, arrowHeadSize = 10.0, color = heTwinColor, lineWidth = 1)


        }
        if (drawHeToPolyLinks) {
          val middle = (origin + ending) * HALF
          val poly = dcel.outerContour(he.leftFace)
          if (he.leftFace != dcel.outerFace && poly.contains(poly.center) && !he.isHoleHalfSide) {
            val polyCenter = poly.center
            val from = utils.math.v2Lerp(middle, polyCenter, 0.1)
            val to = utils.math.v2Lerp(middle, polyCenter, 0.9)
            DrawingUtils.drawArrow(from, to, g, heToPolyColor, 2, 10)
          } else {
            val dir = (ending - origin).rotate90CCW.normalize * 10d
            val from = utils.math.v2Lerp(middle, middle + dir, 0d)
            val to = utils.math.v2Lerp(middle, middle + dir, 4d)
            DrawingUtils.drawArrow(from, to, g, heToPolyColor, 2, 10)
          }
        }
        if (drawHeData) {
          val middle = (origin + ending) * HALF
          DrawingUtils.drawText(he.data.toString, middle, g, 20, false)
        }

      }
    }
  }
}
