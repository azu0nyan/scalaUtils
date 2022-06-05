package drawing.library

import drawing.core.SimpleDrawable
import utils.datastructures.IntV2
import utils.math.Scalar
import utils.math.planar.V2

import java.awt.{Color, Graphics2D}

class DrawableGrid(
                    var min: IntV2 = IntV2(0, 0),
                    var max: IntV2 = IntV2(100, 100),
                    var lineEvery: Int = 10,
                    var pivot: V2 = V2.ZERO,
                    var xAxis: V2 = V2.ox,
                    var yAxis: V2 = V2.oy,
                    var color: Color = Color.BLACK,
                    var lineWidth: Int = 1
                  ) extends SimpleDrawable() {

  override def drawAndUpdate(g: Graphics2D, dt: Scalar): Unit = {
    for (x <- min.x to max.x by lineEvery) DrawingUtils.drawLine(pivot + x * xAxis + min.y * yAxis, pivot + x * xAxis + max.y * yAxis, g, color, lineWidth)
    for (y <- min.y to max.y by lineEvery) DrawingUtils.drawLine(pivot + min.x * xAxis + y * yAxis, pivot + max.x * xAxis + y * yAxis, g, color, lineWidth)
  }
}
