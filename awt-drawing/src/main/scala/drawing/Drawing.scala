package drawing

import java.awt.event.KeyEvent

import drawing.core.{Camera, DrawingWindow}
import drawing.library.{DrawingUtils, FpsCounter}
import utils.abstractions.EnabledDisabled
import utils.datastructures.IntV2

object Drawing extends DrawingWindow {


  def setLogFormat():Unit = {
    System.setProperty("java.util.logging.SimpleFormatter.format",
      "[%1$tF %1$tT] [%4$-7s] %5$s  %n")
  }

  override def startDrawingThread(size: IntV2, decorated: Boolean, camera:Camera = new Camera()): Unit = {

    DrawingUtils.camera = camera


    super.startDrawingThread(size, decorated, camera )
  }

  val  FpsCounter : FpsCounter = new FpsCounter(this)




}
