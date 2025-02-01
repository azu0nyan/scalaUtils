package drawing.library

import drawing.Drawing.{addDrawable, addKeyBinding, log, removeDrawable}

import java.awt.{Color, Font, Graphics2D}
import drawing.core.{DrawingWindow, SimpleDrawable}
import utils.abstractions.EnabledDisabled
import utils.math._

import java.awt.event.KeyEvent

class FpsCounter(d:DrawingWindow) extends SimpleDrawable() with EnabledDisabled{
  var font: Font = new Font("", 0, 15)
  var rowHeight: Int = 20
  var rowNumber: Int = 1
  var dx: Int = 30
  var dy: Int = 30
  var framesCount: Int = 0
  var lastFrames: Seq[Long] = Seq()
  var layer = Int.MaxValue

  override val initialEnabled = false

  addKeyBinding(KeyEvent.VK_F5, toggle() )
  log.info("FpsCounter init ...")
  onEnabled.subscribe { _ =>
    log.info("FpsCounter enabled")
    d.addDrawable(this, layer)
  }
  onDisabled.subscribe{ _ =>
    log.info("FpsCounter disabled")
    d.removeDrawable(this)
  }

  override def drawAndUpdate(g: Graphics2D, dt: Scalar): Unit = {
    this.framesCount += 1
    val currentTime: Long = System.currentTimeMillis
    lastFrames = (lastFrames :+ currentTime).filter(e => (currentTime - e) < 1000)
    g.setColor(Color.RED)
    g.setFont(font)
    g.drawString("fps:" + this.lastFrames.size, this.dx, this.dy + this.rowNumber * this.rowHeight)
  }

  override def toString = s"FpsCounter(${lastFrames.size} fps)"
}
