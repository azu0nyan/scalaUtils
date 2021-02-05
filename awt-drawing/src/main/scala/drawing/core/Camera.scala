package drawing.core

import java.awt.event.{KeyEvent, MouseEvent, MouseMotionListener}

import utils.math._
import utils.math.planar.V2
import utils.system.Event
import utils.system.Event.Event

class Camera(initialLookAt: V2 = new V2(0.0f, 0.0f),
             initialZoom: Int = 37,
             val resolution: V2 = new V2(1920, 1080),
             var zooms: Seq[Scalar] = Seq[Scalar](1.0E-8D, 2.5E-8D, 5.0E-8D, 7.5E-8D, 1.0E-7D, 2.5E-7D, 5.0E-7D, 7.5E-7D, 1.0E-6D, 2.5E-6D, 5.0E-6D, 7.5E-6D, 1.0E-5D, 2.5E-5D, 5.0E-5D, 7.5E-5D, 1.0E-4D, 2.5E-4D, 5.0E-4D, 7.5E-4D, 0.001D, 0.0025D, 0.005D, 0.0075D, 0.001D, 0.0025D, 0.005D, 0.0075D, 0.01D, 0.025D, 0.05D, 0.075D, 0.1D, 0.2D, 0.25D, 0.5D, 0.75D, 1.0D, 1.5D, 2.0D, 2.5D, 3.0D, 4.0D, 5.0D, 7.5D, 10.0D, 15.0D, 20.0D, 30.0D, 40.0D, 50.0D, 70.0D, 100.0D, 150.0D, 200.0D, 250.0D, 400.0D, 500.0D),
             var screenPartPerScroll: Scalar = 0.1f,
             var controlsEnabled: Boolean = false,
             var zoomIN: Int = KeyEvent.VK_E,
             var zoomOUT: Int = KeyEvent.VK_Q,
             var moveUP: Int = KeyEvent.VK_W,
             var moveDOWN: Int = KeyEvent.VK_S,
             var moveLEFT: Int = KeyEvent.VK_A,
             var moveRIGHT: Int = KeyEvent.VK_D,
             var invertY: Boolean = false) {

  private[this] var _cameraZoom: Int = initialZoom
  private[this] def cameraZoom: Int = _cameraZoom
  private[this] def cameraZoom_=(value: Int): Unit = {
    _cameraZoom = value
    _leftTopAngleInWorld = calcLefTopAngleInWorld()
    _zoom = calcZoom
  }

  private def calcZoom: Scalar = zooms(if (cameraZoom >= 0) cameraZoom % zooms.length
  else cameraZoom % zooms.length + zooms.length)

  private[this] var _zoom: Scalar = calcZoom
  def getZoom: Scalar = _zoom


  private[this] var _cameraCenterInWorld: V2 = initialLookAt

  private def cameraCenterInWorld: V2 = _cameraCenterInWorld

  private def calcLefTopAngleInWorld(): V2 = cameraCenterInWorld - screenResolution.normalize * (screenToWorld(screenResolution.length / 2))

  private[this] var _leftTopAngleInWorld: V2 = calcLefTopAngleInWorld()

  def leftTopAngleInWorld: V2 = _leftTopAngleInWorld

  def lookAt(value: V2): Unit = {
    _cameraCenterInWorld = value
    _leftTopAngleInWorld = calcLefTopAngleInWorld()
  }

  var mouseInWorld: V2 = new V2(0.0D, 0.0D)

  var mouseOnScreen: V2 = new V2(0.0D, 0.0D)


  var correspondingWindow: Option[DrawingWindow] = None

  def screenResolution: V2 = resolution


  def worldToScreen(world: Scalar): Scalar = world * getZoom

  def screenToWorld(screen: Scalar): Scalar = screen / getZoom

  def worldToScreen(world: V2): V2 = ((if (invertY) V2(world.x, -world.y) else world) - leftTopAngleInWorld) * getZoom

  def screenToWorld(screen: V2): V2 =
    if (invertY) (V2(screen.x, screen.y)  / getZoom + leftTopAngleInWorld) * ( 1d, -1d) //todo fix
    else screen  / getZoom + leftTopAngleInWorld

  // screen * (1f / getZoom) + leftTopAngleInWorld
//    if (invertY) V2(screen.x, screen.y) * (1f / getZoom) + leftTopAngleInWorld



  def enableControls(): Unit = {
    controlsEnabled = true
  }

  def disableControls(): Unit = {
    controlsEnabled = false
  }

  def updateMousePos(e: MouseEvent): Unit = {
    mouseOnScreen = V2(e.getX, e.getY)
    mouseInWorld = screenToWorld(mouseOnScreen);
    mouseInWorldPosChanged(mouseInWorld)
    mouseOnScreenChangedPos(mouseOnScreen)
  }

  val mouseInWorldPosChanged: Event[V2] = Event[V2]
  val mouseOnScreenChangedPos: Event[V2] = Event[V2]

  def bindToWindow(window: DrawingWindow): Unit = {
    correspondingWindow = Some(window)
    window.addKeyBinding(zoomIN, () => if (controlsEnabled) cameraZoom = Math.min(cameraZoom + 1, zooms.size - 1))
    window.addKeyBinding(zoomOUT, () => if (controlsEnabled) cameraZoom = Math.max(cameraZoom - 1, 0))
    window.addKeyBinding(moveUP, () => if (controlsEnabled) lookAt(cameraCenterInWorld + V2(0, -1) * screenToWorld(screenResolution.length * screenPartPerScroll)))
    window.addKeyBinding(moveDOWN, () => if (controlsEnabled) lookAt(cameraCenterInWorld + V2(0, 1) * screenToWorld(screenResolution.length * screenPartPerScroll)))
    window.addKeyBinding(moveLEFT, () => if (controlsEnabled) lookAt(cameraCenterInWorld + V2(-1, 0) * screenToWorld(screenResolution.length * screenPartPerScroll)))
    window.addKeyBinding(moveRIGHT, () => if (controlsEnabled) lookAt(cameraCenterInWorld + V2(1, 0) * screenToWorld(screenResolution.length * screenPartPerScroll)))

    window.addMouseMotionListener(new MouseMotionListener {
      override def mouseDragged(e: MouseEvent): Unit = {
        updateMousePos(e)
      }

      override def mouseMoved(e: MouseEvent): Unit = {
        updateMousePos(e)
      }
    })
  }


  override def toString = s"Camera($mouseInWorld, $mouseOnScreen, $cameraCenterInWorld, $cameraZoom, $controlsEnabled, $getZoom, $leftTopAngleInWorld)"
}
