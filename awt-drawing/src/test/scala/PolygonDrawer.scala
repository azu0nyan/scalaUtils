import drawing.Drawing
import drawing.core.DrawingWindow
import drawing.library.{DrawableGrid, DrawingUtils}
import utils.datastructures.IntV2
import utils.math.planar.algo.PolygonContains
import utils.math.planar.{Polygon, PolygonRegion, V2}

import java.awt.Font
import java.awt.Color
import java.awt.event.KeyEvent
import java.io.{File, FileOutputStream, PrintWriter}
import java.util.logging.{Level, Logger}
import scala.util.Using

object PolygonDrawer {
  Logger.getLogger("UTILS").setLevel(Level.SEVERE)
  Logger.getLogger("utils.math.planar.algo.PolygonContains$").setLevel(Level.SEVERE)
  var addToParent: Boolean = true

  var parent: Polygon = Polygon(List(PolygonRegion(List(V2(200.0, 0.0), V2(400.0, 0.0), V2(400.0, 200.0), V2(200.0, 200.0), V2(300.0, 100.0))), PolygonRegion(List(V2(0.0, 0.0), V2(200.0, 0.0), V2(100.0, 100.0), V2(100.0, 200.0), V2(0.0, 200.0)))))
  var child: Polygon = Polygon(List())
  var curPoly: Seq[V2] = List()

  var dumpID: Int = 0

  def main(args: Array[String]): Unit = {
    implicit val w: DrawingWindow = Drawing
    Drawing.startDrawingThread(IntV2(1920, 900), true)
    Drawing.setCloseButton()
    //FUCK YOU
    new Thread(() => {
      Thread.sleep(1000)
      Drawing.setSize(Drawing.getWidth, Drawing.getHeight + 1)

    })

    Drawing.addKeyBinding(KeyEvent.VK_2, {
      Drawing.setSize(Drawing.getWidth, Drawing.getHeight + 1)
    })

    Drawing.addKeyBinding(KeyEvent.VK_1, {
      Drawing.setLocation(if (Drawing.getLocation.x == 0) 1920 else 0, 0)
    })
    Drawing.camera.invertY = true
    Drawing.camera.lookAt(V2(0, 100))
    Drawing.FpsCounter.enable()
    Drawing.camera.enableControls()

    val parentColor = new Color(0, 255, 0, 127)
    val childColor = new Color(0, 0, 255, 127)
    val editColor = new Color(255, 0, 0, 127)
    var snapValue = 100

    Drawing.addDrawable(new DrawableGrid(IntV2(-2000, -2000), IntV2(2000, 2000), lineEvery = snapValue), -1000)

    Drawing.addDrawer(g => {
      g.setFont(new Font("", Font.BOLD, 30))
      g.setColor(if (addToParent) parentColor else childColor)
      g.drawString(if (addToParent) "PARENT" else "CHILD", 50, 50)
    })

    Drawing.addDrawer(g => {
      g.setFont(new Font("", Font.BOLD, 30))
      val tmpParent = if (addToParent && curPoly.size >= 3) parent.addRegion(PolygonRegion(curPoly)) else parent
      val tmpChild = if (!addToParent && curPoly.size >= 3) child.addRegion(PolygonRegion(curPoly)) else child
      if (tmpParent.regions.nonEmpty && tmpChild.regions.nonEmpty) {
        var text = ""
        try {
          if (PolygonContains.contains(tmpParent.asSeq, tmpChild.asSeq, false)) text += (" Parent contains child. " +
            (if (PolygonContains.contains(tmpParent.asSeq, tmpChild.asSeq, true)) " NO BORDER CROSSING. " else "BORDER CROSSING"))
        } catch {
          case t: Throwable => text += t.getMessage
        }

        try {
          if (PolygonContains.contains(tmpChild.asSeq, tmpParent.asSeq, false)) text += (" Child contains parent. " +
            (if (PolygonContains.contains(tmpChild.asSeq, tmpParent.asSeq, true)) " NO BORDER CROSSING. " else "BORDER CROSSING"))
          if (PolygonContains.contains(tmpChild.asSeq, tmpParent.asSeq)) text += " Child contains parent. "
        } catch {
          case t: Throwable => text += t.getMessage
        }

        g.drawString(text, 50, 85)
      }
    })


    Drawing.addKeyBinding(KeyEvent.VK_3, {
      addToParent = !addToParent
    })

    def addPolyClick(): Unit = {
      println("Add poly")
      if (curPoly.nonEmpty) {
        if (addToParent) {
          parent = parent.addRegion(PolygonRegion(curPoly))
          curPoly = Seq()
        } else {
          child = child.addRegion(PolygonRegion(curPoly))
          curPoly = Seq()
        }
      }

    }

    Drawing.addKeyBinding(KeyEvent.VK_ENTER, addPolyClick)
    Drawing.addKeyBinding(KeyEvent.VK_SPACE, addPolyClick)
    Drawing.addKeyBinding(KeyEvent.VK_Z,
      if (Drawing.shiftControlAlt.controlPressed && !Drawing.shiftControlAlt.shiftPressed) {
        curPoly = curPoly.dropRight(1)
      } else if (Drawing.shiftControlAlt.controlPressed && Drawing.shiftControlAlt.shiftPressed) {
        curPoly = curPoly.drop(1)
      }
    )
    Drawing.addKeyBinding(KeyEvent.VK_BACK_SPACE, {
      parent = Polygon(parent.regions.reverse.tail.reverse)
    })
    Drawing.addKeyBinding(KeyEvent.VK_DELETE, {
      child = Polygon(child.regions.reverse.tail.reverse)
    })

    Drawing.addKeyBinding(KeyEvent.VK_0, {
      println(s"Dumping current polys id=$dumpID")
      Using(new PrintWriter(new FileOutputStream(new File("GenPolys.txt"), true))) { pw =>
        pw.println(s"-------------$dumpID-------------")
        pw.println(s"var parent: Polygon = $parent")
        pw.println(s"var child: Polygon = $child")
        pw.println(s"var curPoly: Seq[V2] = $curPoly")
        dumpID += 1
      }
    }, true)

    def modPosition(pos: V2): V2 = {
      if (Drawing.shiftControlAlt.shiftPressed) {
        val x = Math.floor((pos + V2(snapValue / 2)).x / snapValue) * snapValue
        val y = Math.floor((pos + V2(snapValue / 2)).y / snapValue) * snapValue
        V2(x, y)
      } else if (Drawing.shiftControlAlt.controlPressed) {
        (parent.vertices ++ child.vertices).minByOption(_.distance(pos)).getOrElse(pos)
      } else if (Drawing.shiftControlAlt.altPressed) {
        (parent.sides ++ child.sides).map(_.clothesPoint(pos)).minByOption(_.distance(pos)).getOrElse(pos)
      } else pos
    }

    Drawing.addDrawer(g => {
      if (Drawing.shiftControlAlt.shiftPressed) {
        val pos = modPosition(Drawing.camera.mouseInWorld)
        DrawingUtils.drawLine(Drawing.camera.mouseInWorld, pos, g, Color.GREEN, 3)
      } else if (Drawing.shiftControlAlt.controlPressed) {
        val pos = modPosition(Drawing.camera.mouseInWorld)
        DrawingUtils.drawLine(Drawing.camera.mouseInWorld, pos, g, Color.RED, 3)
      } else if (Drawing.shiftControlAlt.altPressed) {
        val pos = modPosition(Drawing.camera.mouseInWorld)
        DrawingUtils.drawLine(Drawing.camera.mouseInWorld, pos, g, Color.YELLOW, 3)
      } else {
        DrawingUtils.drawCircle(Drawing.camera.mouseInWorld, 5, g, Color.BLUE, true)
      }
    })

    Drawing.addMouseLeftClickBinding(pos => {

      val point = modPosition(pos)

      curPoly = curPoly :+ point
    })

    Drawing.addMouseRightClickBinding { pos => {
      if (addToParent) {
        parent.regions.find(_.contains(pos)) match {
          case Some(reg) =>
            parent = parent.copy(regions = parent.regions.filter(_ != reg) ++ Option.when(curPoly.size >= 3)(PolygonRegion(curPoly)).toSeq)
            curPoly = reg.vertices
          case None =>
        }
      } else {
        child.regions.find(_.contains(pos)) match {
          case Some(reg) =>
            child = child.copy(regions = child.regions.filter(_ != reg) ++ Option.when(curPoly.size >= 3)(PolygonRegion(curPoly)).toSeq)
            curPoly = reg.vertices
          case None =>
        }
      }
    }
    }

    Drawing.addDrawer(g => {
      for (p <- parent.regions) DrawingUtils.drawPolygon(p, g, true, parentColor)
      for (p <- child.regions) DrawingUtils.drawPolygon(p, g, true, childColor)
      for (p <- parent.regions) DrawingUtils.drawPolygon(p, g, false, Color.BLACK, 3)
      for (p <- child.regions) DrawingUtils.drawPolygon(p, g, false, Color.BLACK, 3)
      curPoly.size match {
        case 0 =>
        case 1 => DrawingUtils.drawCircle(curPoly.head, 2f, g, editColor, true, 2)
        case 2 => DrawingUtils.drawLine(curPoly.head, curPoly.tail.head, g, editColor, 2)
        case _ => DrawingUtils.drawPolygon(PolygonRegion(curPoly), g, true, editColor)
      }


    }, -100)


  }

}
