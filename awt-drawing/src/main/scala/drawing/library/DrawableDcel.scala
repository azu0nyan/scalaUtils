package drawing.library

import drawing.core.SimpleDrawable
import utils.datastructures.dcel.PlanarDCEL
import utils.datastructures.dcel.PlanarDCEL
import utils.math.Scalar
import utils.math.planar.{AngleOps, PolygonRegion, V2}

import java.awt.{Color, Graphics2D}

class DrawableDcel[VD <: V2, HED, FD](
                                 var dcel: PlanarDCEL[VD, HED, FD],
                                 var verticesColor: VD => Color = (x:VD) => new Color(0, 0, 255),
                                 var edgesColor: HED => Color = (x:HED) => new Color(10, 109, 10),
                                 var faceColor: FD => Color =  (x:FD) => new Color(250, 100, 100),
                                 var angleOffset:Scalar = 10d,
                               ) extends SimpleDrawable {
  override def drawAndUpdate(g: Graphics2D, dt: Scalar): Unit = {

    dcel.vertices.foreach{ v =>
      val c = verticesColor(v.data)
      DrawingUtils.drawCircle(dcel.pos(v), angleOffset.toFloat / 2f, g, c, true)

    }

    dcel.innerFaces.foreach { f =>
      val c = faceColor(f.data)
      val vs = f.vertices
      DrawingUtils.drawPolygon(PolygonRegion(vs.map(v => dcel.extractor(v.data)).toSeq), g, true, c)
    }

    dcel.halfEdges.foreach{ he =>
      val color = edgesColor(he.data)
      val p = dcel.pos(he.prev.origin)
      val c = dcel.pos(he.origin)
      val n = dcel.pos(he.next.origin)
      val nn = dcel.pos(he.next.ending)
      val offsetVector = AngleOps.ccwBisectorPath(p, c, n) * angleOffset
      val offsetVector2 = AngleOps.ccwBisectorPath(c, n, nn) * angleOffset
      DrawingUtils.drawArrow(he.origin.data + offsetVector, he.ending.data + offsetVector2, g, arrowHeadSize = 10.0, color = color)
    }
  }
}
