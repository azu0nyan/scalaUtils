package drawing.library

import drawing.core.DrawingWindow

import java.awt.event.{KeyEvent, KeyListener}
import java.util.concurrent.CountDownLatch

object TimeOps {
  def waitKey(keycode: Int)(implicit d: DrawingWindow): Unit = {
    val c = new CountDownLatch(1);

    d.addKeyListener(new KeyListener {
      override def keyTyped(e: KeyEvent): Unit = {}
      override def keyPressed(e: KeyEvent): Unit = {
        if(e.getKeyCode == keycode) {
          c.countDown()
          d.removeKeyListener(this)
        }
      }
      override def keyReleased(e: KeyEvent): Unit = {}
    })
    c.await()


  }
}
