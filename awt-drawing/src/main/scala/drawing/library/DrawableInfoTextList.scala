package drawing.library

import java.awt.{Color, Graphics2D}

import utils.abstractions.EnabledDisabled
import utils.datastructures.spatial.AARectangle
import utils.math._
import utils.math.planar.V2

class DrawableInfoTextList(
                            lines:Seq[String] = Seq(),
                            verticalSpacing:Scalar,
                            borderWidth: Int = 2,
                            val initialPos:V2 = V2.ZERO,
                            var borderColor: Color = Color.BLACK,
                            var bgColor: Color = Color.WHITE,
                            var textColor: Color = Color.GREEN.darker()
                          ) extends FluidMovableToPosition with EnabledDisabled{
  targetPos = initialPos

  def horizontalSize: Scalar = lines.map(_.length).max * 0.5f

  def size: V2 = V2( horizontalSize, lines.size * verticalSpacing + .5f)

  def boxLeftBot: V2 = pos + V2(-size.x, size.y) / 2f

  def boxRightBot: V2 = pos + V2(size.x, size.y) / 2f

  def boxLeftTop: V2 = pos + V2(-size.x, -size.y) / 2f

  def boxRightTop: V2 = pos + V2(size.x, -size.y) / 2f


  /** вызывается в потоке рисования каждый кадр */
  override def draw(g: Graphics2D): Unit = {
    if(enabled) {
      DrawingUtils.drawPolygon(new AARectangle(boxLeftBot, boxRightTop).toPolygon, g, true, bgColor)
      DrawingUtils.drawPolygon(new AARectangle(boxLeftBot, boxRightTop).toPolygon, g, false, borderColor, borderWidth)
      for (i <- lines.indices) {
        val pos = boxLeftTop + V2(.5, 1) + V2(0, verticalSpacing) * i.toFloat
        DrawingUtils.drawText(s"${lines(i)}", pos, g, verticalSpacing * .75f, true, textColor)
      }
    }

  }
}
