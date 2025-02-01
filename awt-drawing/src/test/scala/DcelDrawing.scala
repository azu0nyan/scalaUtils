/* todo
import drawing.Drawing
import drawing.core.DrawingWindow
import drawing.library.{DrawableDcel, DrawingUtils, TimeOps, ToggleableDrawable}
import utils.abstractions.EnabledDisabled
import utils.datastructures.IntV2
import utils.datastructures.dcel.{DCELDataProvider, PlanarDCEL}
import utils.math.planar.{AngleOps, PolygonRegion, V2}
import utils.math._
import utils.system.ConcurrentOps

import java.awt.Color
import java.awt.event.{KeyEvent, MouseEvent, MouseListener}
import java.util.concurrent.atomic.AtomicInteger
import drawing.library.ColorOps._
import utils.datastructures.dcel.DCEL.{DCELData, HalfEdge, Vertex}
import utils.datastructures.spatial.AARectangle
import utils.sugar.SeqOps

import java.io.{File, FileInputStream, FileOutputStream, PrintWriter}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}
import scala.util.Using

object DcelDrawing extends App {
  Using(new PrintWriter(new FileOutputStream(new File("zpolys.txt"), true))) { pw =>
    pw.println(s"--------")
  }

  implicit val w: DrawingWindow = Drawing
  Drawing.startDrawingThread(IntV2(1920, 900), true)
  Drawing.setCloseButton()
  //FUCK YOU
  new Thread(() => {
    Thread.sleep(1000)
    Drawing.setSize(Drawing.getWidth, Drawing.getHeight + 1)

  }).start()

  Drawing.addKeyBinding(KeyEvent.VK_2, {
    Drawing.setSize(Drawing.getWidth, Drawing.getHeight + 1)
  })

  Drawing.addKeyBinding(KeyEvent.VK_1, {
    println(Drawing.getLocation.x)
    Drawing.setLocation(if (Drawing.getLocation.x == 0) 1920 else 0, 0)
  })
  Drawing.camera.invertY = true
  Drawing.camera.lookAt(V2(0, 100))
  Drawing.FpsCounter.enable()
  Drawing.camera.enableControls()

  val pauseOnEvent = new EnabledDisabled {}
  pauseOnEvent.disable()
  def waitIdNeeded(): Unit = {
    if (pauseOnEvent) ConcurrentOps.withoutLock(w.drawUpdateLock) {
      TimeOps.waitKey(KeyEvent.VK_SPACE)
      println(s"Waited")
    }
  }

  type DCELType = DCELData {
    type VertexData = V2
    type HalfEdgeData = Int
    type FaceData = Int
  }
  object DataProvider extends DCELDataProvider[DCELType] {
    override def newVertexData(v: V2): V2 = v
    override def newEdgeData(v1: Vertex[DcelDrawing.DCELType], v2: Vertex[DcelDrawing.DCELType]): (Int, Int) = (heIds.getAndIncrement(), heIds.getAndIncrement())
    override def newFaceData(edge: HalfEdge[DcelDrawing.DCELType]): Int = faceIds.getAndIncrement()
    override def splitEdgeData(edge: HalfEdge[DcelDrawing.DCELType], data: V2): (Int, Int) = (heIds.getAndIncrement(), heIds.getAndIncrement())
  }

  val dcel = new PlanarDCEL[DCELType](0, x => x)


  def faceAt(position: V2): String = dcel.faceAt(position) match {
    case x if x == dcel.outerFace => "outer"
    case x => s"id:${x.data.toString} vs: ${x.vertices.size}"
  }

  val faceDrawer = Drawing.addDrawer(g => {
    DrawingUtils.drawText(faceAt(Drawing.camera.mouseInWorld), Drawing.camera.mouseInWorld, g, 20, false, Color.BLACK)
  }, 100)
  dcel.onNewFace.subscribe(f => {
    println("new face", f.data, f.vertices.size, f.vertices.map(_.data).toSeq)
    waitIdNeeded()
  })
  dcel.onNewHalfEdge.subscribe(e => {
    println(s"new edge:${e.data} twin: ${e.twin.data} position:${dcel.position(e.origin).toProduct}->${dcel.position(e.ending).toProduct} " +
      s"face ${if (e.leftFace == dcel.outerFace) "outer" else e.leftFace.data.toString} " +
      s"twinFace ${if (e.twin.leftFace == dcel.outerFace) "outer" else e.twin.leftFace.data.toString} " +
      s"prev  ${e.prev.data} next ${e.next.data} ${e.twin.prev.data}, ${e.twin.next.data} ")
  })
  dcel.onNewVertex.subscribe(v => {
    val face = dcel.faceAt(dcel.position(v))
    println("new vertex", v.data, dcel.position(v), v.edgesWithOriginHere.size, if (face == dcel.outerFace) "outer" else face.data.toString)
    waitIdNeeded()
  })
  dcel.onHalfEdgeRemoved.subscribe(e => {
    println("edge removed", e.data, e.twin.data, dcel.position(e.origin), dcel.position(e.ending), "leftFace", e.leftFace.data, e.twin.leftFace.data)
    waitIdNeeded()
  })
  dcel.onEdgeSplit.subscribe { case (e1, e2) =>
    println("SPLIT")
    println(s"old edge:${e1.data} twin: ${e1.twin.data} position:${dcel.position(e1.origin).toProduct}->${dcel.position(e1.ending).toProduct} " +
      s"face: ${if (e1.leftFace == dcel.outerFace) "outer" else e1.leftFace.data.toString} " +
      s"twinFace: ${if (e1.twin.leftFace == dcel.outerFace) "outer" else e1.twin.leftFace.data.toString} " +
      s"prev:  ${e1.prev.data} next: ${e1.next.data} ${e1.twin.prev.data}, ${e1.twin.next.data} ")
    println(s"new edge:${e2.data} twin: ${e2.twin.data} position:${dcel.position(e2.origin).toProduct}->${dcel.position(e2.ending).toProduct} " +
      s"face: ${if (e2.leftFace == dcel.outerFace) "outer" else e2.leftFace.data.toString} " +
      s"twinFace: ${if (e2.twin.leftFace == dcel.outerFace) "outer" else e2.twin.leftFace.data.toString} " +
      s"prev:  ${e2.prev.data} next: ${e2.next.data} ${e2.twin.prev.data}, ${e2.twin.next.data} ")
    waitIdNeeded()
  }

  val faceIds = new AtomicInteger()
  val heIds = new AtomicInteger()

  def addPoly(s: Seq[V2]): Future[Unit] = {
    val prom = Promise[Unit]
    new Thread(() => try {
      println("Adding poly")
      val p = if (PolygonRegion(s).isCw) {
        println("Wrong orientation reversing !!!!!!!!")
        s.reverse
      } else s
      Using(new PrintWriter(new FileOutputStream(new File("polys.txt"), true))) { pw =>
        pw.println(s"Await.result(addPoly($s), Duration.Inf)")
      }
      ConcurrentOps.withLock(w.drawUpdateLock) {
        dcel.cutPoly(p, DataProvider)
      }
      //      dcel.halfEdges.foreach(he => println(s"${he.data.toString} ${he.prev.data.toString} ${he.next.data.toString}"))
      println("Added")
      prom.success(())
    } catch {
      case t: Throwable => prom.failure(t)
    }, "Dcel cutting").start()
    prom.future
  }

  def makeEdge(main: V2, toMerge: V2): Future[Unit] = {
    val prom = Promise[Unit]
    new Thread(() => try{
      println("Merging vertices")
      Using(new PrintWriter(new FileOutputStream(new File("polys.txt"), true))) { pw =>
        pw.println(s"Await.result(makeEdge($main, $toMerge), Duration.Inf)")
      }
      ConcurrentOps.withLock(w.drawUpdateLock) {
        val mf = dcel.closestVertexOpt(main).get
        val tmf = dcel.closestVertexOpt(toMerge).get
        dcel.connectVerticesUnsafe(mf, tmf, DataProvider)
      }
      println("Merged")
      prom.success(())


    }catch {
      case t:Throwable => prom.failure(t)
    }).start()
    prom.future
  }

  def mergePoly(main: V2, toMerge: V2): Future[Unit] = {
    val prom = Promise[Unit]
    new Thread(() => try {
      println("Merging poly")
      Using(new PrintWriter(new FileOutputStream(new File("polys.txt"), true))) { pw =>
        pw.println(s"Await.result(mergePoly($main, $toMerge), Duration.Inf)")
      }
      ConcurrentOps.withLock(w.drawUpdateLock) {
        val mf = dcel.faceAt(main)
        val tmf = dcel.faceAt(toMerge)
        dcel.mergeAdjancedFaces(mf, tmf, DataProvider)
      }
      println("Merged")
      prom.success(())
    } catch {
      case t: Throwable => prom.failure(t)
    }, "Dcel merging").start()
    prom.future
  }

  def addChain(s: Seq[V2]): Future[Unit] = {
    val prom = Promise[Unit]
    new Thread(() => try {
      println("Adding chain")

      Using(new PrintWriter(new FileOutputStream(new File("polys.txt"), true))) { pw =>
        pw.println(s"Await.result(addChain($s), Duration.Inf)")
      }
      ConcurrentOps.withLock(w.drawUpdateLock) {
        dcel.cutPoly(s, DataProvider)
      }
      //      dcel.halfEdges.foreach(he => println(s"${he.data.toString} ${he.prev.data.toString} ${he.next.data.toString}"))
      println("Added")
      prom.success(())
    } catch {
      case t: Throwable => prom.failure(t)
    }, "Dcel cutting").start()
    prom.future
  }


  val drawableDcel = Drawing.addDrawable(new DrawableDcel(dcel)).asInstanceOf[DrawableDcel[DCELType]]
  Drawing.addDrawable(new ToggleableDrawable(Some(false), Some(KeyEvent.VK_1), V2(50, 30), V2(30, 30), "PAU", {
    pauseOnEvent.enable()
  }, {
    pauseOnEvent.disable()
  }))
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


  sealed trait Mode
  case object Poly extends Mode
  case object MergePolys extends Mode
  case object MakeEdge extends Mode
  var mode: Mode = Poly

  Drawing.addKeyBinding(KeyEvent.VK_8, {
    mode = mode match {
      case Poly => MergePolys
      case MergePolys => MakeEdge
      case MakeEdge => Poly
    }
    println(s"Current mode $mode")
  })

  //  Drawing.addDrawable(new ToggleableDrawable(Some(false), Some(KeyEvent.VK_8), V2(400, 30), V2(50, 30), "MERGE", {
  //    mode = true
  //  }, {
  //    mode = false
  //  }))

  var poly: Seq[V2] = Seq()

  var snapValue = 100d

  var selectedPoint: Option[V2] = None
  Drawing.addMouseLeftClickBinding(v =>
    mode match {
      case Poly =>
        val point = if (Drawing.shiftControlAlt.shiftPressed) {
          val x = Math.floor((v + V2(snapValue / 2)).x / snapValue) * snapValue
          val y = Math.floor((v + V2(snapValue / 2)).y / snapValue) * snapValue
          V2(x, y)
        } else v
        poly = poly :+ point
      case MergePolys | MakeEdge =>
        println(s"Selected pos at $v")
        selectedPoint = Some(v)
    }  )

  Drawing.addMouseRightClickBinding { v =>
    mode match {
      case Poly =>
        val point = if (Drawing.shiftControlAlt.shiftPressed) {
          val x = Math.floor((v + V2(snapValue / 2)).x / snapValue) * snapValue
          val y = Math.floor((v + V2(snapValue / 2)).y / snapValue) * snapValue

          V2(x, y)
        } else v

        if (Drawing.shiftControlAlt.controlPressed) {
          println(dcel.halfEdges.map(he => (he, dcel.asSegment(he))).filter { case (he, seg) => seg.receiveProjectionFrom(point) }
            .map { case (he, seg) => (he, seg, seg.distanceTo(point)) }.minByOption(_._3))
          dcel.halfEdges.map(he => (he, dcel.asSegment(he))).filter { case (he, seg) => seg.receiveProjectionFrom(point) }
            .map { case (he, seg) => (he, seg, seg.distanceTo(point)) }.minByOption(_._3) match {
            case Some((he, seg, dist)) if (true /*dist < snapValue */) =>
              ConcurrentOps.withLock(w.drawUpdateLock) {
                dcel.split(he, point, heIds.incrementAndGet(), heIds.incrementAndGet())
              }
            case _ =>
          }
        } else {
          addPoly(poly)
          poly = Seq()
        }
      case MergePolys =>
        println(s"Merging polys$selectedPoint $v")
        if (selectedPoint.nonEmpty) mergePoly(selectedPoint.get, v)
      case MakeEdge =>
        println(s"Making edge $selectedPoint $v")
        if(selectedPoint.nonEmpty)makeEdge(selectedPoint.get, v)
    }


  }


  Drawing.addDrawer(g => {
    /* for(x <- 0 until 16; y <- 0 until 16){

       val size = 50
       val v2 = V2(x * size * 1.1 - 400, y * size * 1.1 - 400)
       val v1 = v2 + V2(-size / 2, 0).rotate(TWO_PI * x / 16d)
       val v3 = v2 + V2(size / 2, 0).rotate(TWO_PI * y / 16d)

       val offset = AngleOps.ccwBisectorPath(v1, v2, v3)
       DrawingUtils.drawArrow(v1, v2, g, lineWidth = 2, arrowHeadSize =  size / 5)
       DrawingUtils.drawArrow(v2, v3, g, lineWidth = 2, arrowHeadSize =  size / 5)
       DrawingUtils.drawArrow(v2, v2 + offset * size / 2, g,Color.GREEN, lineWidth = 2, arrowHeadSize =  size / 5)
       DrawingUtils.drawArrow(v2, v2 - offset * size / 2, g,Color.RED, lineWidth = 2, arrowHeadSize =  size / 5)
     }*/


    for (i <- -5 to 5) {
      DrawingUtils.drawLine(V2(500, 100 * i), V2(-500, 100 * i), g, new Color(0, 0, 0, 127), 1)
      DrawingUtils.drawLine(V2(100 * i, 500), V2(100 * i, -500), g, new Color(0, 0, 0, 127), 1)
    }

    DrawingUtils.drawArrow(V2(0, 0), V2(500, 0), g = g, color = new Color(255, 0, 0, 127), arrowHeadSize = 10d, lineWidth = 3)
    DrawingUtils.drawArrow(V2(0, 0), V2(0, 500), g = g, color = new Color(0, 255, 0, 127), arrowHeadSize = 10d, lineWidth = 3)
  }, -100)


  /*
    Await.result(addPoly(List(V2(-200.0, -100.0), V2(200.0, -100.0), V2(200.0, 200.0), V2(-200.0, 200.0))), Duration.Inf)
    Await.result(addPoly(List(V2(-100.0, 0.0), V2(0.0, 0.0), V2(0.0, 100.0), V2(-100.0, 100.0))), Duration.Inf)
    Await.result(addPoly(List(V2(0.0, 0.0), V2(100.0, 0.0), V2(100.0, 100.0), V2(0.0, 100.0))), Duration.Inf)
    pauseOnEvent.enable()
  //  waitIdNeeded()
    Await.result(mergePoly(V2(-36.0, -62.0), V2(-46.0, 51.0)), Duration.Inf)
  */
  /*
    Await.result(addPoly(List(V2(452.0, 448.0), V2(450.8284271247462, 445.1715728752538), V2(448.0, 444.0), V2(448.0, 452.0), V2(450.8284271247462, 450.8284271247462)).map(v => (v - V2(450, 450)) * 20)), Duration.Inf)
    Await.result(addPoly(List(V2(464.0, 448.0), V2(452.0, 448.0), V2(450.8284271247462, 450.8284271247462), V2(448.0, 452.0), V2(448.0, 464.0), V2(459.31370849898474, 459.31370849898474)).map(v => (v - V2(450, 450)) * 20)), Duration.Inf)
    Await.result(addPoly(List(V2(468.0, 448.0), V2(462.14213562373095, 433.85786437626905), V2(448.0, 428.0), V2(433.85786437626905, 433.85786437626905), V2(428.0, 448.0), V2(433.85786437626905, 462.14213562373095), V2(448.0, 468.0), V2(462.14213562373095, 462.14213562373095)).map(v => (v - V2(450, 450)) * 20)), Duration.Inf)
    Await.result(addPoly(List(V2(459.31370849898474, 459.31370849898474), V2(448.0, 464.0), V2(436.68629150101526, 459.31370849898474), V2(432.0, 448.0), V2(436.68629150101526, 436.68629150101526), V2(448.0, 432.0), V2(459.31370849898474, 436.68629150101526), V2(464.0, 448.0)).map(v => (v - V2(450, 450)) * 20)), Duration.Inf)

    Await.result(addPoly(List(V2(464.0, 448.0), V2(459.31370849898474, 436.68629150101526), V2(448.0, 432.0), V2(448.0, 436.0), V2(439.5147186257614, 439.5147186257614), V2(436.0, 448.0), V2(444.0, 448.0), V2(445.1715728752538, 445.1715728752538), V2(448.0, 444.0), V2(450.8284271247462, 445.1715728752538), V2(452.0, 448.0)).map(v => (v - V2(450, 450)) * 20)), Duration.Inf)
    Await.result(addPoly(List(V2(448.0, 452.0), V2(448.0, 444.0), V2(445.1715728752538, 445.1715728752538), V2(444.0, 448.0), V2(445.1715728752538, 450.8284271247462)).map(v => (v - V2(450, 450)) * 20)), Duration.Inf)
    Await.result(addPoly(List(V2(448.0, 436.0), V2(448.0, 432.0), V2(436.68629150101526, 436.68629150101526), V2(432.0, 448.0), V2(436.0, 448.0), V2(439.5147186257614, 439.5147186257614)).map(v => (v - V2(450, 450)) * 20)), Duration.Inf)
    Await.result(addPoly(List(V2(448.0, 464.0), V2(448.0, 452.0), V2(445.1715728752538, 450.8284271247462), V2(444.0, 448.0), V2(432.0, 448.0), V2(436.68629150101526, 459.31370849898474)).map(v => (v - V2(450, 450)) * 20)), Duration.Inf)
  */

  println()
}
*/