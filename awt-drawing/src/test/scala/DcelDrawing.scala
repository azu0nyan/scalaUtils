import drawing.Drawing
import drawing.core.DrawingWindow
import drawing.library.{DrawableDcel, DrawingUtils, TimeOps}
import utils.abstractions.EnabledDisabled
import utils.datastructures.IntV2
import utils.datastructures.dcel.PlanarDCEL
import utils.math.planar.{AngleOps, PolygonRegion, V2}
import utils.math._
import utils.system.ConcurrentOps

import java.awt.Color
import java.awt.event.{KeyEvent, MouseEvent, MouseListener}
import java.util.concurrent.atomic.AtomicInteger
import drawing.library.ColorOps._

import java.io.{File, FileInputStream, FileOutputStream, PrintWriter}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}
import scala.util.Using

object DcelDrawing extends App {
  Using(new PrintWriter(new FileOutputStream(new File("polys.txt"), true))) { pw =>
    pw.println(s"--------")
  }

  implicit val w: DrawingWindow = Drawing
  Drawing.startDrawingThread(IntV2(1920, 900), true)
  Drawing.setCloseButton()
  //FUCK YOU
  new Thread(() => {
    Thread.sleep(100)
    Drawing.setSize(Drawing.getWidth, Drawing.getHeight + 1)
  }).start()

  Drawing.addKeyBinding(KeyEvent.VK_F1, () => {
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
    }
  }
  Drawing.addKeyBinding(KeyEvent.VK_1, () => pauseOnEvent.toggle())


  val dcel = new PlanarDCEL[V2, Int, Int](0, x => x)


  def faceAt(pos: V2): String = dcel.faceAt(pos) match {
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
  dcel.onNewEdge.subscribe(e => {
    println(s"new edge:${e.data} twin: ${e.twin.data} pos:${dcel.pos(e.origin).toProduct}->${dcel.pos(e.ending).toProduct} " +
      s"face ${if (e.leftFace == dcel.outerFace) "outer" else e.leftFace.data.toString} " +
      s"twinFace ${if (e.twin.leftFace == dcel.outerFace) "outer" else e.twin.leftFace.data.toString} " +
      s"prev  ${e.prev.data} next ${e.next.data} ${e.twin.prev.data}, ${e.twin.next.data} ")
  })
  dcel.onNewVertex.subscribe(v => {
    val face = dcel.faceAt(dcel.pos(v))
    println("new vertex", v.data, dcel.pos(v), v.edgesWithOriginHere.size, if (face == dcel.outerFace) "outer" else face.data.toString)
    waitIdNeeded()
  })
  dcel.onEdgeCollapse.subscribe(e => {
    println("edge collapse", e.data, e.twin.data, dcel.pos(e.origin), dcel.pos(e.ending), "leftFace", e.leftFace.data, e.twin.leftFace.data)
    waitIdNeeded()
  })
  dcel.onEdgeSplit.subscribe { case (e1, e2) =>
    println("SPLIT")
    println(s"old edge:${e1.data} twin: ${e1.twin.data} pos:${dcel.pos(e1.origin).toProduct}->${dcel.pos(e1.ending).toProduct} " +
      s"face: ${if (e1.leftFace == dcel.outerFace) "outer" else e1.leftFace.data.toString} " +
      s"twinFace: ${if (e1.twin.leftFace == dcel.outerFace) "outer" else e1.twin.leftFace.data.toString} " +
      s"prev:  ${e1.prev.data} next: ${e1.next.data} ${e1.twin.prev.data}, ${e1.twin.next.data} ")
    println(s"new edge:${e2.data} twin: ${e2.twin.data} pos:${dcel.pos(e2.origin).toProduct}->${dcel.pos(e2.ending).toProduct} " +
      s"face: ${if (e2.leftFace == dcel.outerFace) "outer" else e2.leftFace.data.toString} " +
      s"twinFace: ${if (e2.twin.leftFace == dcel.outerFace) "outer" else e2.twin.leftFace.data.toString} " +
      s"prev:  ${e2.prev.data} next: ${e2.next.data} ${e2.twin.prev.data}, ${e2.twin.next.data} ")
    waitIdNeeded()
  }

  val faceIds = new AtomicInteger()
  val heIds = new AtomicInteger()

  def addPoly(s: Seq[V2]): Future[Unit] = {
    val prom = Promise[Unit]
    new Thread(() => {
      println("Adding poly")
      val p = if (PolygonRegion(s).isCw) {
        println("Wrong orientation reversing !!!!!!!!")
        s.reverse
      } else s
      Using(new PrintWriter(new FileOutputStream(new File("polys.txt"), true))) { pw =>
        pw.println(s"Await.result(addPoly($s), Duration.Inf)")
      }
      ConcurrentOps.withLock(w.drawUpdateLock) {
        dcel.cutPoly(p, x => x, (x, y) => (heIds.getAndIncrement(), heIds.getAndIncrement()), (x, y) => (heIds.getAndIncrement(), heIds.getAndIncrement()), x => faceIds.getAndIncrement())
      }
      //      dcel.halfEdges.foreach(he => println(s"${he.data.toString} ${he.prev.data.toString} ${he.next.data.toString}"))
      println("Added")
      prom.success(())
    }, "Dcel cutting").start()
    prom.future
  }


  //  Drawing.addDrawer(g => {
  //
  //    g.setColor(Color.BLACK)
  //    g.drawLine(200, 2000, 300, 100)
  //  })

  //  val f = dcel.makeFace(1)
  //  val v1 = dcel.makeVertex(V2(100, 100))
  //  val v2 = dcel.makeVertex(V2(200, 500))
  //  val v3 = dcel.makeVertex(V2(500, 500))
  //  dcel.makeEdge(v1, v2, f, dcel.outerFace, ids.getAndIncrement(), ids.getAndIncrement())
  //  dcel.makeEdge(v2, v3, f, dcel.outerFace, ids.getAndIncrement(), ids.getAndIncrement())
  //  dcel.makeEdge(v3, v1, f, dcel.outerFace, ids.getAndIncrement(), ids.getAndIncrement())
  //

  //  addPoly(Seq(V2(0, 0), V2(0, 100), V2(100, 100)).map(x => x * 1d - 2 * V2(100, 100)))


  Drawing.addDrawable(new DrawableDcel(dcel))


  var poly: Seq[V2] = Seq()

  var snapValue = 100d


  Drawing.addMouseLeftClickBinding(v => {
    val point = if (Drawing.shiftControlAlt.shiftPressed) {
      val x = Math.floor((v + V2(snapValue / 2)).x / snapValue) * snapValue
      val y = Math.floor((v + V2(snapValue / 2)).y / snapValue) * snapValue

      V2(x, y)
    } else v

    poly = poly :+ point
  }
  )
  Drawing.addMouseRightClickBinding { v =>
    val point = if (Drawing.shiftControlAlt.shiftPressed) {
      val x = Math.floor((v + V2(snapValue / 2)).x / snapValue) * snapValue
      val y = Math.floor((v + V2(snapValue / 2)).y / snapValue) * snapValue

      V2(x, y)
    } else v

    if (Drawing.shiftControlAlt.controlPressed) {
      println(dcel.halfEdges.map(he => (he, dcel.seg(he))).filter { case (he, seg) => seg.receiveProjectionFrom(point) }
        .map { case (he, seg) => (he, seg, seg.distanceTo(point)) }.minByOption(_._3))
      dcel.halfEdges.map(he => (he, dcel.seg(he))).filter { case (he, seg) => seg.receiveProjectionFrom(point) }
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

  //  Await.result(addPoly(List(V2(-300.0, -100.0), V2(300.0, -100.0), V2(300.0, 200.0), V2(-300.0, 200.0))), Duration.Inf)
  //  Await.result(addPoly(List(V2(-100.0, 300.0), V2(-100.0, 100.0), V2(300.0, -100.0), V2(300.0, 200.0), V2(100.0, 200.0), V2(100.0, 300.0))), Duration.Inf)
  //  pauseOnEvent.enable()
  //  Await.result(addPoly(List(V2(0.0, -100.0), V2(0.0, 100.0), V2(-100.0, 100.0), V2(-300.0, -100.0))), Duration.Inf)
  Await.result(addPoly(List(V2(0.0, 100.0), V2(0.0, 300.0), V2(-100.0, 300.0))), Duration.Inf)
  Await.result(addPoly(List(V2(0.0, 100.0), V2(-100.0, 300.0), V2(-200.0, 300.0))), Duration.Inf)
  Await.result(addPoly(List(V2(0.0, 100.0), V2(-200.0, 100.0), V2(-200.0, 0.0))), Duration.Inf)
  Await.result(addPoly(List(V2(0.0, 100.0), V2(200.0, 100.0), V2(200.0, 200.0))), Duration.Inf)
  pauseOnEvent.enable()
  Await.result(addPoly(List(V2(0.0, 100.0), V2(0.0, -100.0), V2(100.0, -100.0))), Duration.Inf)
  //  Await.result(addPoly(List(V2(0.0, 100.0), V2(200.0, 200.0), V2(0.0, 300.0))), Duration.Inf)
  //  Await.result(addPoly(List(V2(0.0, 100.0), V2(100.0, -100.0), V2(200.0, 100.0))), Duration.Inf)
  //  dcel.
}
