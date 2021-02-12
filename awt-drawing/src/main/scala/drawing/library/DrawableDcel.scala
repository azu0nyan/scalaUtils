package drawing.library

import drawing.core.SimpleDrawable
import drawing.library.ColorOps._
import drawing.library.ColorOps
import utils.datastructures.dcel.DCEL.RawHalfEdge
import utils.datastructures.dcel.PlanarDCEL
import utils.datastructures.dcel.PlanarDCEL
import utils.math.{HALF, Scalar}
import utils.math.planar.{AngleOps, PolygonRegion, V2}

import java.awt.{Color, Graphics2D}

class DrawableDcel[VD <: V2, HED, FD](
                                       var dcel: PlanarDCEL[VD, HED, FD],
                                       var verticesColor: VD => Color = (x: VD) => new Color(0, 0, 255),
                                       var edgesColor: HED => Color = (x: HED) => new Color(10, 129, 10),
                                       var faceColor: FD => Color = (x: FD) => {
                                         import drawing.library.ColorOps._
                                         import drawing.library.ColorOps.ColorOps
                                         randomColor(x.##).setAlpha(30)
                                       },
                                       var angleOffset: Scalar = 15d,
                                       var drawHeData: Boolean = true,
                                       var drawHeConnections: Boolean = true,
                                       var hePrevColor: Color = new Color(30, 30, 230),
                                       var heNextPrevColor: Color = new Color(235, 30, 30),
                                       var heFaceColor: Color = new Color(30, 230, 30)
                                     ) extends SimpleDrawable {
  override def drawAndUpdate(g: Graphics2D, dt: Scalar): Unit = {

    dcel.vertices.foreach { v =>
      val c = verticesColor(v.data)
      DrawingUtils.drawCircle(dcel.pos(v), angleOffset.toFloat / 4f, g, c, true)

    }

    dcel.innerFaces.foreach { f =>
      val c = faceColor(f.data)
      val vs = f.vertices
      DrawingUtils.drawPolygon(PolygonRegion(vs.map(v => dcel.extractor(v.data)).toSeq), g, true, c)
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

    dcel.halfEdges.foreach { he =>
      val color = edgesColor(he.data)
      val (origin, ending) = fromTo(he)
      DrawingUtils.drawArrow(origin, ending, g, arrowHeadSize = 20.0, color = color, lineWidth = 2)

      if (drawHeData) {
        val middle = (origin + ending) * HALF
        DrawingUtils.drawText(he.data.toString, middle, g, 20, false)
      }
      if (drawHeConnections) {
        val (prevOrigin, _) = fromTo(he.prev)
        val (_, nextEnding) = fromTo(he.next)
        val toPrevStart = utils.math.v2Lerp(origin, ending, 0.05)
        val toPrevEnd = utils.math.v2Lerp(prevOrigin, origin, 0.95)

        val toNextStart = utils.math.v2Lerp(origin, ending, 0.9)
        val toNextEnd = utils.math.v2Lerp(ending, nextEnding, 0.1)

        DrawingUtils.drawArrow(toPrevStart, toPrevEnd, g, arrowHeadSize = 10.0, color = hePrevColor, lineWidth = 1)
        DrawingUtils.drawArrow(toNextStart, toNextEnd, g, arrowHeadSize = 10.0, color = heNextPrevColor, lineWidth = 1)


      }
    }
  }
}
