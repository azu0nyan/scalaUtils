import drawing.Drawing
import drawing.core.DrawingWindow
import drawing.library.{DrawableDcel, DrawingUtils, ToggleableDrawable}
import utils.datastructures.IntV2
import utils.datastructures.dcel.DCEL.{DCELData, HalfEdge, Vertex}
import utils.datastructures.dcel.PlanarDCELCutPipeline.{CutChain, Labels}
import utils.datastructures.dcel.{DCELDataProvider, PlanarDCEL, PlanarDcelCutPipelineInterpreter}
import utils.math.planar.V2

import java.awt.Color
import java.awt.event.KeyEvent
import java.util.concurrent.atomic.AtomicInteger

object DcelCutTester extends App {
  Drawing.startDrawingThread(IntV2(1920, 900), true)
  Drawing.setCloseButton()
  //FUCK YOU
  new Thread(() => {
    Thread.sleep(1000)
    Drawing.setSize(Drawing.getWidth, Drawing.getHeight + 1)

  }).start()

  Drawing.camera.invertY = true
  Drawing.camera.lookAt(V2(0, 100))
  Drawing.FpsCounter.enable()
  Drawing.camera.enableControls()
  implicit val w: DrawingWindow = Drawing

  type DATA = DCELData {
    type VertexData = V2
    type HalfEdgeData = Int
    type FaceData = Int
  }
  type Ls = Labels {
    type VertexLabel = Int
    type HalfEdgeLabel = Int
    type FaceLabel = Int
  }

  val faceIds = new AtomicInteger()
  val heIds = new AtomicInteger()
  object Provider extends DCELDataProvider[DATA] {
    override def newVertexData(v: V2): V2 = v
    override def newEdgeData(v1: Vertex[DcelDrawing.DCELType], v2: Vertex[DcelDrawing.DCELType]): (Int, Int) = (heIds.getAndIncrement(), heIds.getAndIncrement())
    override def newFaceData(edge: HalfEdge[DcelDrawing.DCELType]): Int = faceIds.getAndIncrement()
    override def splitEdgeData(edge: HalfEdge[DcelDrawing.DCELType], data: V2): (Int, Int) = (heIds.getAndIncrement(), heIds.getAndIncrement())
  }

  val dcel = new PlanarDCEL[DATA](0, x => x)
  val op1 = CutChain[DATA, Ls](Seq(V2(0, 200), V2(0, -200), V2(200, 0), V2(-200, 0)),
    Seq(1, 2, 3), Seq(4, 5, 6), Seq(-1, -2, -3, -4))

  val res = PlanarDcelCutPipelineInterpreter.cutPipeline(dcel, Provider, op1)




  def faceAt(pos: V2): String = dcel.faceAt(pos) match {
    case x if x == dcel.outerFace => "outer"
    case x => s"id:${x.data.toString} vs: ${x.vertices.size}"
  }

  val faceDrawer = Drawing.addDrawer(g => {
    DrawingUtils.drawText(faceAt(Drawing.camera.mouseInWorld), Drawing.camera.mouseInWorld, g, 20, false, Color.BLACK)
  }, 100)

  val drawableDcel = Drawing.addDrawable(new DrawableDcel(dcel)).asInstanceOf[DrawableDcel[DATA]]

  Drawing.addDrawable(new ToggleableDrawable(Some(true), Some(KeyEvent.VK_2), V2(100, 30), V2(30, 30), "IDS", {
    drawableDcel.drawHeData = true
  }, {
    drawableDcel.drawHeData = false
  }))
  Drawing.addDrawable(new ToggleableDrawable(Some(true), Some(KeyEvent.VK_3), V2(150, 30), V2(30, 30), "HEC", {
    drawableDcel.drawHeConnections = true
  }, {
    drawableDcel.drawHeConnections = false
  }))
  Drawing.addDrawable(new ToggleableDrawable(Some(true), Some(KeyEvent.VK_4), V2(200, 30), V2(30, 30), "POL", {
    drawableDcel.drawHeToPolyLinks = true
  }, {
    drawableDcel.drawHeToPolyLinks = false
  }))
  Drawing.addDrawable(new ToggleableDrawable(Some(false), Some(KeyEvent.VK_5), V2(250, 30), V2(30, 30), "VTS", {
    drawableDcel.drawVertexConnections = true
  }, {
    drawableDcel.drawVertexConnections = false
  }))
  Drawing.addDrawable(new ToggleableDrawable(Some(true), Some(KeyEvent.VK_6), V2(300, 30), V2(30, 30), "BOR", {
    drawableDcel.drawPolyBorders = true
  }, {
    drawableDcel.drawPolyBorders = false
  }))
  Drawing.addDrawable(new ToggleableDrawable(Some(true), Some(KeyEvent.VK_7), V2(350, 30), V2(30, 30), "HE", {
    drawableDcel.drawHalfEdges = true
  }, {
    drawableDcel.drawHalfEdges = false
  }))
  Drawing.addDrawer(g => {

    for (i <- -5 to 5) {
      DrawingUtils.drawLine(V2(500, 100 * i), V2(-500, 100 * i), g, new Color(0, 0, 0, 127), 1)
      DrawingUtils.drawLine(V2(100 * i, 500), V2(100 * i, -500), g, new Color(0, 0, 0, 127), 1)
    }

    DrawingUtils.drawArrow(V2(0, 0), V2(500, 0), g = g, color = new Color(255, 0, 0, 127), arrowHeadSize = 10d, lineWidth = 3)
    DrawingUtils.drawArrow(V2(0, 0), V2(0, 500), g = g, color = new Color(0, 255, 0, 127), arrowHeadSize = 10d, lineWidth = 3)
  }, -100)



}
