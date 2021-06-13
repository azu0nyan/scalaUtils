package drawing.library

import java.awt.{BasicStroke, Color, Font, Graphics2D}

import drawing.core.Camera
import utils.datastructures.spatial.AARectangle
import utils.math.planar.{PolygonRegion, SegmentPlanar, V2}
import utils.math.{EIGHT_PI, Scalar}

object DrawingUtils {

  var camera: Camera = new Camera

  def drawPolygon(p: PolygonRegion, g: Graphics2D, fill: Boolean = false, color: Color = Color.BLACK, lineWidth: Int = 1): Unit = {
    val transformed = p.vertices.map(
      v => camera.worldToScreen(v)
    )
    val xs = transformed.map(v => v.xInt).toArray
    val ys = transformed.map(v => v.yInt).toArray
    g.setColor(color)
    g.setStroke(new BasicStroke(lineWidth.toFloat))
    if (fill) {
      g.fillPolygon(xs, ys, xs.length)
    } else {
      g.drawPolygon(xs, ys, xs.length)
    }
  }
  def drawText(text: String, where: V2, g: Graphics2D, fontSize: Scalar, scaleFont: Boolean = true, color: Color = Color.BLACK, fontName: String = "Lucida Console", fontType: Int = Font.PLAIN): Unit = {
    g.setColor(color)
    g.setFont(new Font(fontName, fontType, if (scaleFont) camera.worldToScreen(fontSize).toInt else fontSize.toInt))
    g.drawString(text, camera.worldToScreen(where).xInt, camera.worldToScreen(where).yInt)
  }

  def drawCircle(where: V2, radius: Float, g: Graphics2D, color: Color = Color.BLACK, fill: Boolean = false, lineWidth: Int = 2): Unit = {
    val rad = camera.worldToScreen(radius).toInt
    val wh = camera.worldToScreen(where)
    g.setColor(color)
    if (fill) {
      g.fillOval(wh.xInt - rad, wh.yInt - rad, 2 * rad, 2 * rad)
    } else {
      g.setStroke(new BasicStroke(lineWidth.toFloat))
      g.drawOval(wh.xInt - rad, wh.yInt - rad, 2 * rad, 2 * rad)
    }
  }

  def drawRect(rect: AARectangle, g: Graphics2D, color: Color = Color.BLACK, fill: Boolean = false, lineWidth: Int = 2): Unit = {
    val w = camera.worldToScreen(rect.width).toInt
    val h = camera.worldToScreen(rect.height).toInt
    val xy = camera.worldToScreen(rect.min).toIntV2
    //todo fix for inverted y
    g.setColor(color)
    if (fill) {
      g.fillRect(xy.x, xy.y, w, h )
    } else {
      g.setStroke(new BasicStroke(lineWidth.toFloat))
      g.drawRect(xy.x, xy.y, w, h )
    }
  }

  def drawPoint(where: V2, radius: Float, g: Graphics2D, color: Color = Color.BLACK, fill: Boolean = false, pointWidth: Int = 5): Unit = {
    val rad = pointWidth / 2
    val wh = camera.worldToScreen(where)
    g.setColor(color)
    if (fill) {
      g.fillOval(wh.xInt - rad, wh.yInt - rad, 2 * rad, 2 * rad)
    } else {
      g.setStroke(new BasicStroke(1))
      g.drawOval(wh.xInt - rad, wh.yInt - rad, 2 * rad, 2 * rad)
    }
  }

  def drawSegment(s: SegmentPlanar, g: Graphics2D, color: Color = Color.BLACK, lineWidth: Int = 1): Unit = {
    drawLine(s.v1, s.v2, g, color, lineWidth)
  }

  def drawArrow(from: V2, to: V2, g: Graphics2D, color: Color = Color.BLACK, lineWidth: Int = 1, arrowHeadSize: Scalar = 0.5f): Unit = {
    val f = camera.worldToScreen(from)
    val t = camera.worldToScreen(to)
    val tfDist = f.distance(t)
    val tfDelta: V2 = (f - t).normalize * math.min(camera.worldToScreen(arrowHeadSize) , tfDist * .5).toFloat
    val arrowEnd1 = tfDelta.rotate(EIGHT_PI) + t
    val arrowEnd2 = tfDelta.rotate(-EIGHT_PI) + t

    g.setColor(color)
    g.setStroke(new BasicStroke(lineWidth.toFloat))
    g.drawLine(arrowEnd1.xInt, arrowEnd1.yInt, t.xInt, t.yInt)
    g.drawLine(arrowEnd2.xInt, arrowEnd2.yInt, t.xInt, t.yInt)
    g.drawLine(f.xInt, f.yInt, t.xInt, t.yInt)
  }
  def drawLine(from: V2, to: V2, g: Graphics2D, color: Color = Color.BLACK, lineWidth: Int = 1): Unit = {
    val f = camera.worldToScreen(from)
    val t = camera.worldToScreen(to)
    g.setColor(color)
    g.setStroke(new BasicStroke(lineWidth.toFloat))
    g.drawLine(f.xInt, f.yInt, t.xInt, t.yInt)
  }
}
