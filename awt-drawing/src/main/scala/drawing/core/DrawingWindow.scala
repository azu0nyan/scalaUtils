package drawing.core

import java.awt.event._
import java.awt.{Graphics2D, Toolkit}
import java.util.concurrent.{ConcurrentSkipListMap, CopyOnWriteArrayList, Semaphore}
import java.util.logging.{Level, Logger}
import drawing.library.DrawingUtils

import javax.swing.{JFrame, WindowConstants}
import utils.datastructures.IntV2

import scala.jdk.CollectionConverters._
import utils.math._
import utils.math.planar.V2
import utils.system.ConcurrentOps
import utils.system.Event.Listener

import java.util.concurrent.locks.ReentrantLock

class DrawingWindow() extends JFrame {


  val log: Logger = Logger.getLogger("DrawingWindow")
  //setExtendedState(6)
  setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

  var camera: Camera = _

  var shiftControlAlt = new ShiftControlAltListener

  private var lastFrameEnd = 0L

  private var startTime = 0L

  private val drawableObjectsDepthMap: ConcurrentSkipListMap[Int, CopyOnWriteArrayList[DrawableUpdatable]] = new ConcurrentSkipListMap[Int, CopyOnWriteArrayList[DrawableUpdatable]]()

  private[this] var _fpsLimit: Int = 60

  private def fpsLimit: Int = _fpsLimit

  private def fpsLimit_=(value: Int): Unit = {
    _fpsLimit = value
  }

  def startDrawingThread(size: IntV2 = IntV2(1920, 1080), decorated: Boolean = true, c: Camera = new Camera()): Unit = {
    camera = c
    camera.bindToWindow(this)
    setSize(size.x, size.y)
    setUndecorated(!decorated)
    setVisible(true)
    createBufferStrategy(2)
    lastFrameEnd = System.currentTimeMillis
    startTime = System.currentTimeMillis
    addKeyListener(shiftControlAlt)

    val drawingThread = new Thread(
      () => {
        Thread.sleep(100)
        log.info("Drawing thread started")
        while (true) {
          val dtMs = System.currentTimeMillis - lastFrameEnd
          lastFrameEnd = System.currentTimeMillis
          updateAndDraw(dtMs.toDouble / 1000.0D)
          val frameLength = System.currentTimeMillis - lastFrameEnd
          if (_fpsLimit != 0 && frameLength < (1000 / _fpsLimit).toLong) {
            Thread.sleep((1000 / _fpsLimit).toLong - frameLength)
          }
        }
      }
      , "UpdateAndDrawThread")
    drawingThread.start()
  }

  def removeDrawable(drawable: DrawableUpdatable): Unit = {
    drawableObjectsDepthMap.values().forEach(v => v.remove(drawable))
    log.log(Level.INFO, s"Object removed : $drawable   drawables total: ${drawableObjectsDepthMap.values().asScala.map(_.size()).sum}")
  }

  def addDrawer(d: Graphics2D => Unit, depth: Int = 0, visible:Boolean = true): DrawableObject = {
    val toAdd = new DrawableObject(drawMe = d, visible = visible)
    addDrawable(toAdd, depth)
    return toAdd
  }

  def addDrawable[T<:DrawableUpdatable](drawable: T, depth: Int = 0): T = {
    drawableObjectsDepthMap
      .computeIfAbsent(depth, _ => new CopyOnWriteArrayList[DrawableUpdatable]())
      .add(drawable)
    log.log(Level.INFO, s"Object : $drawable with depth: $depth added, drawables total: ${drawableObjectsDepthMap.values().asScala.map(_.size()).sum}")
    return drawable
  }

  def changeDepth(drawable: DrawableUpdatable, newDepth: Int): Unit = {
    removeDrawable(drawable)
    addDrawable(drawable, newDepth)
  }

  def getAllObject: Iterable[DrawableUpdatable] = {
    drawableObjectsDepthMap.asScala.values.flatMap(layer => layer.asScala.toSeq)
  }

  val drawUpdateLock:ReentrantLock = new ReentrantLock(true)
  private def updateAndDraw(dt: Scalar): Unit = {
    val bs = getBufferStrategy
    val g2d = bs.getDrawGraphics.asInstanceOf[Graphics2D]
    g2d.clearRect(0, 0, getWidth, getHeight)
    ConcurrentOps.withLock(drawUpdateLock) {
      updateAndDrawObjects(g2d, dt)
    }
    g2d.dispose()
    bs.show()
    Toolkit.getDefaultToolkit.sync()
  }

  def updateAndDrawObjects(g: Graphics2D, dt: Scalar): Unit = {
    drawableObjectsDepthMap.values().forEach(_.forEach(updateObject(_, dt)))
    drawableObjectsDepthMap.values().forEach(_.forEach(drawObject(_, g)))
  }

  def updateObject(drawable: DrawableUpdatable, dt: Scalar): Unit = {
    try {
      drawable.update(dt)
    } catch {
      case e: Exception =>
        println(drawable)
        e.printStackTrace()
    }
  }

  def drawObject(drawable: DrawableUpdatable, g: Graphics2D): Unit = {
    try {
      drawable.draw(g)
    } catch {
      case e: Exception =>
        println(drawable)
        e.printStackTrace()
    }
  }


  /**
    * @param keyCode используйте KeyEvent.VK_<КЛАВИША> чтобы указать какая клавиша нажата
    * @param pressed true событие реагирует на нажате клавиши, false на отпускание
    * @param action
    */
  def addKeyBinding(keyCode: Int, action: => Unit, pressed: Boolean = true): KeyListener = {
    val listener = new KeyListener {
      override def keyTyped(e: KeyEvent): Unit = {}

      override def keyPressed(e: KeyEvent): Unit = {
        if (pressed && e.getKeyCode == keyCode) {
          action
        }
      }

      override def keyReleased(e: KeyEvent): Unit = {
        if (!pressed && e.getKeyCode == keyCode) {
          action
        }
      }
    }
    addKeyListener(listener)
    return listener
  }


  def addMouseMovedListener(f:V2 => Unit):Listener[V2] = {
    return camera.mouseInWorldPosChanged.subscribe(f)
  }
  /**
    * @param f takes mouse in world
    * @return handle used to delete binding
    */
  def addMouseLeftClickBinding(f: V2 => Unit): MouseListener = addMouseClickBinding(f, MouseEvent.BUTTON1)

  /**
    * @param f takes mouse in world
    * @return handle used to delete binding
    */
  def addMouseRightClickBinding(f: V2 => Unit): MouseListener = addMouseClickBinding(f, MouseEvent.BUTTON3)
  /**
    * @param f takes mouse in world
    * @return handle used to delete binding
    */
  def addMouseMiddleClickBinding(f: V2 => Unit): MouseListener = addMouseClickBinding(f, MouseEvent.BUTTON2)
  /**
    * @param f takes mouse in world
    * @return handle used to delete binding
    */
  def addMouseClickBinding(f: V2 => Unit, button: Int = MouseEvent.BUTTON1): MouseListener = {
    val listener = new MouseAdapter {
      override def mouseReleased(e: MouseEvent): Unit = {
        if (e.getButton == button) {
          camera.updateMousePos(e)
          f.apply(camera.mouseInWorld)
        }
      }
    }
    addMouseListener(listener)
    return listener
  }

  def setCloseButton(keycode:Int = KeyEvent.VK_ESCAPE):KeyListener  = {
    addKeyBinding(keycode, System.exit(0))
  }

  val mousePosDrawer:DrawableObject = addDrawer(g =>{
    DrawingUtils.drawText(f"${camera.mouseInWorld.x}%.2f ${camera.mouseInWorld.y}%.2f ", camera.mouseInWorld, g, 20, scaleFont = false)
  }, 10000, false)

}
