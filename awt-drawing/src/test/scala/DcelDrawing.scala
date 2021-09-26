import drawing.Drawing
import drawing.core.DrawingWindow
import drawing.library.{DrawableDcel, DrawingUtils, TimeOps, ToggleableDrawable}
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

  Drawing.addKeyBinding(KeyEvent.VK_F2, {
    Drawing.setSize(Drawing.getWidth, Drawing.getHeight + 1)
  })

  Drawing.addKeyBinding(KeyEvent.VK_F1, {
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
  dcel.onNewHalfEdge.subscribe(e => {
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
  dcel.onHalfEdgeCollapse.subscribe(e => {
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
        dcel.cutPoly(p, x => x, (x, y) => (heIds.getAndIncrement(), heIds.getAndIncrement()), (x, y) => (heIds.getAndIncrement(), heIds.getAndIncrement()), x => faceIds.getAndIncrement())
      }
      //      dcel.halfEdges.foreach(he => println(s"${he.data.toString} ${he.prev.data.toString} ${he.next.data.toString}"))
      println("Added")
      prom.success(())
    } catch {
      case t: Throwable => prom.failure(t)
    }, "Dcel cutting").start()
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
        dcel.cutPoly(s, x => x, (x, y) => (heIds.getAndIncrement(), heIds.getAndIncrement()), (x, y) => (heIds.getAndIncrement(), heIds.getAndIncrement()), x => faceIds.getAndIncrement())
      }
      //      dcel.halfEdges.foreach(he => println(s"${he.data.toString} ${he.prev.data.toString} ${he.next.data.toString}"))
      println("Added")
      prom.success(())
    } catch {
      case t: Throwable => prom.failure(t)
    }, "Dcel cutting").start()
    prom.future
  }


  val drawableDcel = Drawing.addDrawable(new DrawableDcel(dcel)).asInstanceOf[DrawableDcel[V2, Int, Int]]
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



  //  Await.result(addPoly(AARectangle(V2(-64*10, -64*10), V2(64*10, 64*10)).toPolygon.vertices), Duration.Inf)
  //  Await.result(addPoly(AARectangle(V2(-64*10, -64*10), V2(64*10, 64*10)).toPolygon.vertices), Duration.Inf)
  //  pauseOnEvent.enable()
  //  Await.result(addChain(Seq(V2(-16.5*10, 31.20927970918268*10) ,  V2(38.96171633544622*10, -64.0*10))), Duration.Inf)
  //  Await.result(addChain(Seq(V2(19.5*10, 28.20927970918268*10) ,  V2(-33.31890779458037*10, -64.0*10))), Duration.Inf)
  //  Await.result(addChain(Seq(V2(29.0*10, 1.709279709182681*10) ,  V2(-18.634080222389713*10, 64.0*10))), Duration.Inf)
  //
  //  Await.result(addPoly(List(V2(0.0, 100.0), V2(0.0, 300.0), V2(-100.0, 300.0))), Duration.Inf)
  //  Await.result(addPoly(List(V2(0.0, 100.0), V2(-100.0, 300.0), V2(-200.0, 300.0))), Duration.Inf)
  //  Await.result(addPoly(List(V2(0.0, 100.0), V2(-200.0, 100.0), V2(-200.0, 0.0))), Duration.Inf)
  //  Await.result(addPoly(List(V2(0.0, 100.0), V2(200.0, 100.0), V2(200.0, 200.0))), Duration.Inf)
  //  Await.result(addPoly(List(V2(0.0, 100.0), V2(0.0, -100.0), V2(100.0, -100.0))), Duration.Inf)
  //  Await.result(addPoly(List(V2(100.0, 300.0), V2(100.0, 0.0), V2(-100.0, -100.0), V2(-100.0, -200.0), V2(300.0, -200.0), V2(300.0, 300.0))), Duration.Inf)
  //  Await.result(addPoly(List(V2(-300.0, 200.0), V2(0.0, 200.0), V2(100.0, 200.0), V2(100.0, 300.0), V2(100.0, 400.0), V2(-200.0, 400.0))), Duration.Inf)


  //Worlks
  //  Await.result(addPoly(AARectangle(V2(-64 * 100, -64 * 100), V2(64 * 100, 64 * 100)).toPolygon.vertices), Duration.Inf)
  //  pauseOnEvent.enable()
  //
  //  Await.result(addPoly(List(V2(320.0, -320.0), V2(320.0, -324.0), V2(322.8284271247462, -322.8284271247462), V2(324.0, -320.0), V2(322.8284271247462, -317.1715728752538), V2(320.0, -316.0), V2(320.0, -320.0), V2(320.0, -320.0), V2(320.0, -320.0), V2(320.0, -320.0)).map(v => (v - V2(300, -300)) * 20)), Duration.Inf)
  //  Await.result(addPoly(List(V2(324.0, -320.0), V2(328.0, -320.0), V2(332.0, -320.0), V2(336.0, -320.0), V2(331.31370849898474, -308.68629150101526), V2(320.0, -304.0), V2(320.0, -308.0), V2(320.0, -312.0), V2(320.0, -316.0), V2(322.8284271247462, -317.1715728752538)).map(v => (v - V2(300, -300)) * 20)), Duration.Inf)
  //  Await.result(addPoly(List(V2(320.0, -304.0), V2(320.0, -300.0), V2(305.85786437626905, -305.85786437626905), V2(300.0, -320.0), V2(305.85786437626905, -334.14213562373095), V2(320.0, -340.0), V2(334.14213562373095, -334.14213562373095), V2(340.0, -320.0), V2(334.14213562373095, -305.85786437626905), V2(320.0, -300.0), V2(320.0, -304.0), V2(331.31370849898474, -308.68629150101526), V2(336.0, -320.0), V2(331.31370849898474, -331.31370849898474), V2(320.0, -336.0), V2(308.68629150101526, -331.31370849898474), V2(304.0, -320.0), V2(308.68629150101526, -308.68629150101526)).map(v => (v - V2(300, -300)) * 20)), Duration.Inf)
  //  Await.result(addPoly(List(V2(316.0, -320.0), V2(312.0, -320.0), V2(308.0, -320.0), V2(311.5147186257614, -328.4852813742386), V2(320.0, -332.0), V2(320.0, -336.0), V2(331.31370849898474, -331.31370849898474), V2(336.0, -320.0), V2(332.0, -320.0), V2(328.0, -320.0), V2(324.0, -320.0), V2(322.8284271247462, -322.8284271247462), V2(320.0, -324.0), V2(317.1715728752538, -322.8284271247462)).map(v => (v - V2(300, -300)) * 20)), Duration.Inf)
  //  Await.result(addPoly(List(V2(320.0, -320.0), V2(320.0, -316.0), V2(317.1715728752538, -317.1715728752538), V2(316.0, -320.0), V2(317.1715728752538, -322.8284271247462), V2(320.0, -324.0), V2(320.0, -320.0), V2(320.0, -320.0), V2(320.0, -320.0), V2(320.0, -320.0)).map(v => (v - V2(300, -300)) * 20)), Duration.Inf)
  //  Await.result(addPoly(List(V2(308.0, -320.0), V2(304.0, -320.0), V2(308.68629150101526, -331.31370849898474), V2(320.0, -336.0), V2(320.0, -332.0), V2(311.5147186257614, -328.4852813742386)).map(v => (v - V2(300, -300)) * 20)), Duration.Inf)
  //  Await.result(addPoly(List(V2(320.0, -316.0), V2(320.0, -312.0), V2(320.0, -308.0), V2(320.0, -304.0), V2(308.68629150101526, -308.68629150101526), V2(304.0, -320.0), V2(308.0, -320.0), V2(312.0, -320.0), V2(316.0, -320.0), V2(317.1715728752538, -317.1715728752538)).map(v => (v - V2(300, -300)) * 20)), Duration.Inf)

  //Await.result(addPoly(AARectangle(V2(-64 * 100, -64 * 100), V2(64 * 100, 64 * 100)).toPolygon.vertices), Duration.Inf)
 // pauseOnEvent.enable()
  Await.result(addPoly(List(V2(452.0, 448.0), V2(450.8284271247462, 445.1715728752538), V2(448.0, 444.0), V2(448.0, 452.0), V2(450.8284271247462, 450.8284271247462)).map(v => (v - V2(450, 450)) * 20)), Duration.Inf)
  Await.result(addPoly(List(V2(464.0, 448.0), V2(452.0, 448.0), V2(450.8284271247462, 450.8284271247462), V2(448.0, 452.0), V2(448.0, 464.0), V2(459.31370849898474, 459.31370849898474)).map(v => (v - V2(450, 450)) * 20)), Duration.Inf)
  Await.result(addPoly(List(V2(468.0, 448.0), V2(462.14213562373095, 433.85786437626905), V2(448.0, 428.0), V2(433.85786437626905, 433.85786437626905), V2(428.0, 448.0), V2(433.85786437626905, 462.14213562373095), V2(448.0, 468.0), V2(462.14213562373095, 462.14213562373095)).map(v => (v - V2(450, 450)) * 20)), Duration.Inf)
  Await.result(addPoly(List(V2(459.31370849898474, 459.31370849898474), V2(448.0, 464.0), V2(436.68629150101526, 459.31370849898474), V2(432.0, 448.0), V2(436.68629150101526, 436.68629150101526), V2(448.0, 432.0), V2(459.31370849898474, 436.68629150101526), V2(464.0, 448.0)).map(v => (v - V2(450, 450)) * 20)), Duration.Inf)

  Await.result(addPoly(List(V2(464.0, 448.0), V2(459.31370849898474, 436.68629150101526), V2(448.0, 432.0), V2(448.0, 436.0), V2(439.5147186257614, 439.5147186257614), V2(436.0, 448.0), V2(444.0, 448.0), V2(445.1715728752538, 445.1715728752538), V2(448.0, 444.0), V2(450.8284271247462, 445.1715728752538), V2(452.0, 448.0)).map(v => (v - V2(450, 450)) * 20)), Duration.Inf)
  Await.result(addPoly(List(V2(448.0, 452.0), V2(448.0, 444.0), V2(445.1715728752538, 445.1715728752538), V2(444.0, 448.0), V2(445.1715728752538, 450.8284271247462)).map(v => (v - V2(450, 450)) * 20)), Duration.Inf)
  Await.result(addPoly(List(V2(448.0, 436.0), V2(448.0, 432.0), V2(436.68629150101526, 436.68629150101526), V2(432.0, 448.0), V2(436.0, 448.0), V2(439.5147186257614, 439.5147186257614)).map(v => (v - V2(450, 450)) * 20)), Duration.Inf)
  Await.result(addPoly(List(V2(448.0, 464.0), V2(448.0, 452.0), V2(445.1715728752538, 450.8284271247462), V2(444.0, 448.0), V2(432.0, 448.0), V2(436.68629150101526, 459.31370849898474)).map(v => (v - V2(450, 450)) * 20)), Duration.Inf)

  println()
}
