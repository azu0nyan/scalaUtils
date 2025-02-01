package drawing.library

import java.awt.{Color, Graphics2D}

import drawing.core.SimpleDrawable
import utils.math.Scalar
import utils.math.planar.{PolygonRegion, SegmentPlanar, V2}

object DrawableShapes{
  class DrawablePolygon(var p: PolygonRegion, var color: Color = Color.BLACK, var fill: Boolean= false, var lineWidth: Int = 1) extends SimpleDrawable()  {

    override def drawAndUpdate(g: Graphics2D, dt: Scalar): Unit = DrawingUtils.drawPolygon(p, g, fill, color, lineWidth)
  }

  class DrawableSegment(var s:SegmentPlanar,var color: Color = Color.BLACK, var lineWidth: Int = 1) extends SimpleDrawable(){
    override def drawAndUpdate(g: Graphics2D, dt: Scalar): Unit =     DrawingUtils.drawSegment(s, g,color, lineWidth)

  }

  class DrawablePoint(var v:V2, var color: Color = Color.BLACK,var fill: Boolean= false, var lineWidth: Int = 1) extends SimpleDrawable(){
    override def drawAndUpdate(g: Graphics2D, dt: Scalar): Unit =     DrawingUtils.drawCircle(v, 0.1f, g, color, fill, lineWidth)
  }
}
