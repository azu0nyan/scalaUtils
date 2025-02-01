package drawing.library

import drawing.core.{DrawableUpdatable, DrawingWindow}
import utils.abstractions.EnabledDisabled
import utils.datastructures.spatial.AARectangle
import utils.math.Scalar
import utils.math.planar.V2

import java.awt.event.{MouseEvent, MouseListener}
import java.awt.{Color, Font, Graphics2D}

class ToggleableDrawable( val initial:Option[Boolean],
                          val hotkey: Option[Int],
                          var pos: V2,
                          var size: V2,
                          var text: String,
                          enableAction: => Unit,
                          disableAction: => Unit,
                          var enabledColor: Color = new Color(0, 205, 0),
                          var disabledColor: Color = new Color(50, 55, 50),
                          var bgColor: Color = new Color(20, 20, 20, 20),
                        )(implicit w: DrawingWindow) extends EnabledDisabled with DrawableUpdatable {
  onEnabled.subscribe(e => enableAction)
  onDisabled.subscribe(e => disableAction)


  initial.foreach(b => if(b) enable() else disable())
  hotkey.foreach { hk =>
    w.addKeyBinding(hk, toggle())
  }


  def area:AARectangle= AARectangle(pos, pos + size)
  w.addDrawable(this, 200)
  w.addMouseListener(new MouseListener {
    override def mouseClicked(e: MouseEvent): Unit = {
      if(area.contains(V2(e.getX, e.getY))) {
        toggle()
      }
    }
    override def mousePressed(e: MouseEvent): Unit = {}
    override def mouseReleased(e: MouseEvent): Unit = {}
    override def mouseEntered(e: MouseEvent): Unit = {}
    override def mouseExited(e: MouseEvent): Unit = {}
  })
  /** вызывается в потоке рисования каждый кадр */
  override def draw(g: Graphics2D): Unit = {
    g.setColor(bgColor)
    g.drawRect(pos.xInt, pos.yInt, size.xInt, size.yInt)
    g.setColor(if (isEnabledNow) enabledColor else disabledColor)
    g.setFont(new Font("Lucida console", Font.BOLD, size.yInt / 2))
    val bs = g.getFont.getStringBounds(text, g.getFontRenderContext)
    //    val tx = bs.getWidth
    g.drawString(text, (pos.xInt + (size.x - bs.getWidth) / 2).toInt, pos.yInt + size.y.toInt * 3 / 4)
  }
  /** вызывается до рисования каждый кадр */
  override def update(dt: Scalar): Unit = {}
}
