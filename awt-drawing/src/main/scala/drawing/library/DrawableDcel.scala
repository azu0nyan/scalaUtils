package drawing.library

import drawing.core.SimpleDrawable
import utils.datastructures.dcel.PlanarDCEL
import utils.datastructures.dcel.PlanarDCEL
import utils.math.Scalar
import utils.math.planar.PolygonRegion

import java.awt.{Color, Graphics2D}

class DrawableDcel[VD, HED, FD](
                                 var dcel: PlanarDCEL[VD, HED, FD],
                                 var verticesColor: VD => Color = (x:VD) => new Color(0, 0, 255),
                                 var edgesColor: HED => Color = (x:HED) => new Color(0, 255, 0),
                                 var faceColor: FD => Color =  (x:FD) => new Color(250, 100, 100)
                               ) extends SimpleDrawable {
  override def drawAndUpdate(g: Graphics2D, dt: Scalar): Unit = {
    dcel.innerFaces.foreach { f =>
      val c = faceColor(f.data)
      DrawingUtils.drawPolygon(PolygonRegion(f.vertices.map(v => dcel.extractor(v.data)).toSeq), g, true, c)
    }

    dcel.halfEdges.foreach{ he =>
      val c = edgesColor(he.data)


    }
  }
}
