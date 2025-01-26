package utils.math.planar.algo.straightSkeleton.debugDrawing

import drawing.Drawing
import drawing.library.DrawingUtils
import utils.datastructures.CircullarOps
import utils.datastructures.CircullarOps.asCyclicPairs
import utils.math.Scalar
import utils.math.planar.algo.straightSkeleton.{Corner, Edge, Machine, OffsetSkeleton}
import utils.math.planar.algo.straightSkeleton.implhelpers.{Loop, LoopL}
import utils.math.planar.{PolygonRegion, V2}
import utils.math.space.V3

import java.awt.event.KeyEvent
import java.awt.{Color, Graphics2D}


@main
def main(): Unit = {
  Drawing.startDrawingThread()


  var points: Seq[V2] = Seq()

  var offset: Scalar = 10

  Drawing.addKeyBinding(KeyEvent.VK_2, {
    offset += 5
  })

  Drawing.addKeyBinding(KeyEvent.VK_1, {
    offset -= 5
  })

  Drawing.addMouseLeftClickBinding(pos =>
    if (Drawing.shiftControlAlt.noModsPressed) {
      points = points :+ pos
    }
  )

  Drawing.addMouseRightClickBinding(pos =>
    if (Drawing.shiftControlAlt.noModsPressed) {
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
      applyDrawAlgo(g)
    }
  })


  def applyDrawAlgo(g: Graphics2D): Unit = {
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


}

