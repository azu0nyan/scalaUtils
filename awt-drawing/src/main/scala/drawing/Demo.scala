package drawing

import java.awt.Color
import java.awt.event.KeyEvent

object Demo extends App {
  Drawing.startDrawingThread()
  val d = Drawing.addDrawer(g => {
    g.setColor(Color.RED)
    g.drawLine(100, 100, 400, 400)
  })
  Drawing.addKeyBinding(KeyEvent.VK_SPACE, () => d.toggle())


}
