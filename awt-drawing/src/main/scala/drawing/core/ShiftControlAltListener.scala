package drawing.core

import java.awt.event.{KeyEvent, KeyListener}

class ShiftControlAltListener extends KeyListener {
  var shiftPressed: Boolean = false
  var altPressed: Boolean = false
  var controlPressed: Boolean = false

  override def keyTyped(e: KeyEvent): Unit = {

  }
  override def keyPressed(e: KeyEvent): Unit = {
    e.getKeyCode match {
      case KeyEvent.VK_SHIFT => shiftPressed = true
      case KeyEvent.VK_ALT => altPressed = true
      case KeyEvent.VK_CONTROL => controlPressed = true
      case _ =>
    }
  }
  override def keyReleased(e: KeyEvent): Unit = {
    e.getKeyCode match {
      case KeyEvent.VK_SHIFT => shiftPressed = false
      case KeyEvent.VK_ALT => altPressed = false
      case KeyEvent.VK_CONTROL => controlPressed = false
      case _ =>
    }
  }

  def anyModsPressed: Boolean = shiftPressed || altPressed || controlPressed

  def noModsPressed: Boolean = !anyModsPressed
}
