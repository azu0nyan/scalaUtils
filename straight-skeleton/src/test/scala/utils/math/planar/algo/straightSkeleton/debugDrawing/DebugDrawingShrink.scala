package utils.math.planar.algo.straightSkeleton.debugDrawing

import drawing.Drawing
import drawing.library.{DrawableGrid, DrawingUtils}
import utils.datastructures.{CircullarOps, IntV2}
import utils.datastructures.CircullarOps.asCyclicPairs
import utils.math.Scalar
import utils.math.planar.algo.straightSkeleton.helpers.SSOps
import utils.math.planar.algo.straightSkeleton.{Corner, Edge, Machine, OffsetSkeleton}
import utils.math.planar.algo.straightSkeleton.implhelpers.{Loop, LoopL}
import utils.math.planar.{PolygonRegion, V2}
import utils.math.space.V3

import java.awt.event.KeyEvent
import java.awt.{Color, Graphics2D}
import java.io.{File, FileOutputStream, PrintWriter}
import java.time.{Instant, LocalDateTime}
import scala.util.{Failure, Success, Try}


@main
def main(): Unit = {
  Drawing.startDrawingThread()
  Drawing.camera.invertY = true

  val debugOut = new PrintWriter(new FileOutputStream(new File("out.txt"), true), true)
  debugOut.println(s"-" * 100)
  debugOut.println(s"new run at: ${LocalDateTime.now().toString}")


  var points: Seq[V2] = Seq(
    V2(600.0, -100.0),
    V2(600.0, 0.0),
    V2(500.0, 0.0),
    V2(500.0, 400.0),
    V2(400.0, 400.0),
    V2(400.0, -100.0),
    V2(500.0, -200.0),
  )

  var offset: Scalar = 10

  val gridSize: Scalar = 100d

  Drawing.addKeyBinding(KeyEvent.VK_2, {
    offset += 5
  })

  Drawing.addKeyBinding(KeyEvent.VK_1, {
    offset -= 5
  })

  Drawing.addDrawable(new DrawableGrid(
    min = IntV2(-1000, -1000),
    max = IntV2(1000, 1000),
    lineEvery = gridSize.toInt,
    color = new Color(100, 100, 100, 100)
  ))

  Drawing.addMouseLeftClickBinding(pos =>
    if (Drawing.shiftControlAlt.noModsPressed) {
      points = points :+ pos
      debugOut.println(s" $pos")
    } else if (Drawing.shiftControlAlt.shiftPressed) {
      val newPos = ((pos + V2(gridSize / 2, gridSize / 2)) / gridSize).floor * gridSize
      points = points :+ newPos
      println(s"$pos $newPos")
      debugOut.println(s" $newPos")
    }
  )

  Drawing.addMouseRightClickBinding(pos =>
    if (Drawing.shiftControlAlt.noModsPressed) {
      debugOut.println(s"remove point: ${points.lastOption.getOrElse("None")}")
      points = points.dropRight(1)
    }
  )

  Drawing.addDrawer(g => {
    val pold = points
    for (p <- pold)
      DrawingUtils.drawCircle(p, 1f, g, Color.BLUE, true)


    if (points.size == 2) {
      DrawingUtils.drawLine(points(0), points(1), g, Color.RED, 3)
    } else if (points.size >= 3) {
      DrawingUtils.drawPolygon(PolygonRegion(points), g, true, new Color(0, 255, 0, 100))

      try {
        applyDrawShrinkAlgo(g)
        applyDrawPolys(g)
      } catch {
        case e: Throwable =>
          e.printStackTrace()
      }
    }
  })


  def applyDrawShrinkAlgo(g: Graphics2D): Unit = {
    val loop1 = new Loop[Edge]()

    val corners = points.map { case V2(x, y) => new Corner(x, y) }

    val edges = corners.asCyclicPairs.map { case (c1, c2) => new Edge(c1, c2) }

    val directionMachine = new Machine()

    for (e <- edges)
      e.machine = directionMachine
      loop1.append(e)

    val a = new LoopL[Edge](loop1)
    val output = OffsetSkeleton.shrink(a, offset)

    val res: Seq[Seq[V3]] = output.iterator.map(_.iterator.toSeq.map(_.asInstanceOf[Corner].asV3)).toSeq

    val resMapped = res
      .map(_.map(_.dropZ))
      .map(PolygonRegion.apply)

    for {
      p <- resMapped
    } {
      DrawingUtils.drawPolygon(p, g, false, new Color(0, 255, 0))
    }
  }

  def applyDrawPolys(g: Graphics2D): Unit = {
    val skel = SSOps.calculcateFor(points.map(_.toProduct): _ *)
    val faces = SSOps.dumpFaces(skel)

    //    val maxHeight = faces.flatMap(_.map(_.z)).max
    //

    for {
      f <- faces
    } {
      DrawingUtils.drawPolygon(PolygonRegion(f.map(_.dropZ)), g, false, new Color(255, 0, 0))
    }


    //    for{f <- faces;
    //      (s, e) <- CircullarOps        .toCyclicPairs(f)} {
    //      
    //    }


  }


}

