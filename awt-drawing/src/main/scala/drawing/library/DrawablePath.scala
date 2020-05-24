package drawing.library

import java.awt.{Color, Graphics2D}

import drawing.core.DrawableUpdatable
import utils.math.planar.patch.Path.Path
import utils.math._

class DrawablePath(
                    var p:Path,
                    var segments:Int = 10,
                    var width:Int = 1,
                    var color:Color = Color.GREEN
                  ) extends DrawableUpdatable {
  /** вызывается в потоке рисования каждый кадр */
  override def draw(g: Graphics2D): Unit = {
    p.toPoints(segments).sliding(2).foreach{ se =>
      DrawingUtils.drawLine(se(0), se(1), g, color, width)
    }
  }
  /** вызывается до рисования каждый кадр */
  override def update(dt: Scalar): Unit = {}
}