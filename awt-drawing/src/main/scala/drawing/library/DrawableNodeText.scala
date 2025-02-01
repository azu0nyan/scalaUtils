package drawing.library

import java.awt.{Color, Graphics2D}

import drawing.core.SimpleDrawable
import utils.datastructures.spatial.AARectangle
import utils.math.planar.V2
import utils.math._

object DrawableNodeText {
  type ArrowStart[T] = DrawableNodeText[T] => V2
  type ArrowEnd[T] = T => Option[V2]

  type Arrow[T] = (Color, ArrowStart[T], ArrowEnd[T])

  implicit class ArrowHelper[T](val a: Arrow[T]) extends AnyVal {
    def color: Color = a._1

    def start(tb: DrawableNodeText[T]): V2 = a._2.apply(tb)

    def end(t: T): Option[V2] = a._3.apply(t)
  }


  val noArrow: Arrow[Any] = (Color.BLACK, _ => V2.ZERO, _ => None)

  def topArrow[T](ae: ArrowEnd[T], c: Color = Color.BLACK): Arrow[T] = (c, b => b.topConnection, ae)

  def botArrow[T](ae: ArrowEnd[T], c: Color = Color.BLACK): Arrow[T] = (c, b => b.botConnection, ae)

  def centerLeftBotArrow[T](ae: ArrowEnd[T], c: Color = Color.BLACK): Arrow[T] = (c, b => b.centerLeftBotConnection, ae)

  def centerRightBotLeftArrow[T](ae: ArrowEnd[T], c: Color = Color.BLACK): Arrow[T] = (c, b => b.centerRightBotConnection, ae)

  def leftBotArrow[T](ae: ArrowEnd[T], c: Color = Color.BLACK): Arrow[T] = (c, b => b.boxLeftBot, ae)

  def leftTopArrow[T](ae: ArrowEnd[T], c: Color = Color.BLACK): Arrow[T] = (c, b => b.boxLeftTop, ae)

  def rightBotArrow[T](ae: ArrowEnd[T], c: Color = Color.BLACK): Arrow[T] = (c, b => b.boxRightBot, ae)

  def rightTopArrow[T](ae: ArrowEnd[T], c: Color = Color.BLACK): Arrow[T] = (c, b => b.boxRightTop, ae)

  class DrawableTextBoxArrows[T](
                                  var drawableTextBox: DrawableNodeText[T],
                                  var arrows: Seq[Arrow[T]]
                                ) extends SimpleDrawable {
    override def drawAndUpdate(g: Graphics2D, dt: Scalar): Unit = {
      arrows.foreach { a =>
        a.end(drawableTextBox.value).foreach { end =>
          val start = a.start(drawableTextBox)
          DrawingUtils.drawArrow(start, end, g, a.color)
        }
      }
    }
  }

}

class DrawableNodeText[T](
                          val value: T,
                          var valueText: T => String = {(v:T) => v.toString},
                          var valueTextColor: Color = Color.BLACK,
                          var smallTextColor: Color = Color.BLACK,
                          var leftText: T => String = {(v:T) => ""},
                          var rightText: T => String = {(v:T) => ""},
                          var botText: T => String = {(v:T) => ""},
                          var size: V2 = V2(5, 3),
                          borderWidth: Int = 2,
                          var borderColor: Color = Color.BLACK,
                          var bgColor: Color = Color.WHITE
                        ) extends FluidMovableToPosition {


  def topConnection: V2 = pos + V2(0, -size.y) / 2f

  def botConnection: V2 = pos - V2(0, -size.y) / 2f

  def centerLeftBotConnection: V2 = pos + V2(-size.x / 4, size.y / 2f)

  def centerRightBotConnection: V2 = pos + V2(size.x / 4, size.y / 2f)

  def boxLeftBot: V2 = pos + V2(-size.x, -size.y) / 2f

  def boxRightBot: V2 = pos + V2(size.x, -size.y) / 2f

  def boxLeftTop: V2 = pos + V2(-size.x, size.y) / 2f

  def boxRightTop: V2 = pos + V2(size.x, size.y) / 2f

  def myNextConnection: V2 = boxRightTop

  def myPrevConnection: V2 = boxLeftBot

  def toMeNextConnections: V2 = boxLeftTop

  def toMePrevConnections: V2 = boxRightBot

  def valueTextPos: V2 = pos + V2(-size.x * .4f, size.y / 3f)

  def leftTextPos: V2 = pos + V2(-size.x * .4f, -size.y * .1f)

  def rightTextPos: V2 = pos + V2(size.x * .2f / 4, -size.y * .1f)

  def botTextPos: V2 = pos + V2(-size.x * .4f, -size.y / 4f)

  /** вызывается в потоке рисования каждый кадр */
  override def draw(g: Graphics2D): Unit = {
    DrawingUtils.drawPolygon(new AARectangle(boxLeftBot, boxRightTop).toPolygon, g, true, bgColor)
    DrawingUtils.drawPolygon(new AARectangle(boxLeftBot, boxRightTop).toPolygon, g, false, borderColor, borderWidth)
    DrawingUtils.drawText(s"${valueText(value)}", valueTextPos, g, size.y / 2.5f, true, valueTextColor)
    DrawingUtils.drawText(s"${botText(value)}", botTextPos, g, size.y / 4f, true, smallTextColor)
    DrawingUtils.drawText(s"${leftText(value)}", leftTextPos, g, size.y / 4f, true, smallTextColor)
    DrawingUtils.drawText(s"${rightText(value)}", rightTextPos, g, size.y / 4f, true, smallTextColor)
  }
}
