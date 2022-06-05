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
import scala.util.Using

object PolygonDrawer {
  var addToParent: Boolean = false
  var curPoly: Seq[V2] = Seq()
  var curParent: Polygon = Polygon(Seq())
  var curChild: Polygon = Polygon(Seq())
  var dumpID:Int = 0

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
      val tmpParent = if(addToParent && curPoly.size >= 3) curParent.addRegion(PolygonRegion(curPoly)) else curParent
      val tmpChild = if(!addToParent && curPoly.size >= 3) curChild.addRegion(PolygonRegion(curPoly)) else curChild
      if(tmpParent.regions.nonEmpty && tmpChild.regions.nonEmpty) {
        var text = ""
        try{
          if(PolygonContains.contains(tmpParent.asSeq, tmpChild.asSeq)) text += " Parent contains child. "
        } catch {
          case t: Throwable => text += t.getMessage
        }

        try{
          if(PolygonContains.contains(tmpChild.asSeq, tmpParent.asSeq)) text += " Child contains parent. "
        } catch {
          case t: Throwable => text += t.getMessage
        }

        g.drawString(text, 50, 85)
      }
    })


    Drawing.addKeyBinding(KeyEvent.VK_3, {
      addToParent = !addToParent
    })

    Drawing.addKeyBinding(KeyEvent.VK_ENTER, {
      println("ENTER")
      if (curPoly.nonEmpty) {
        if (addToParent) {
          curParent = curParent.addRegion(PolygonRegion(curPoly))
          curPoly = Seq()
        } else {
          curChild = curChild.addRegion(PolygonRegion(curPoly))
          curPoly = Seq()
        }
      }


    }, true)

    Drawing.addKeyBinding(KeyEvent.VK_0, {
      println(s"Dumping current polys id=$dumpID")
      Using(new PrintWriter(new FileOutputStream(new File("GenPolys.txt"), true))) { pw =>
        pw.println(s"-------------$dumpID-------------")
        pw.println(s"var parent: Polygon = $curParent")
        pw.println(s"var child: Polygon = $curChild")
        dumpID += 1
      }
    }, true)
    Drawing.addMouseLeftClickBinding(pos => {

      val point = if (Drawing.shiftControlAlt.shiftPressed) {
        val x = Math.floor((pos + V2(snapValue / 2)).x / snapValue) * snapValue
        val y = Math.floor((pos + V2(snapValue / 2)).y / snapValue) * snapValue

        V2(x, y)
      } else pos
      curPoly = curPoly :+ point
    })


    Drawing.addDrawer(g => {
      for (p <- curParent.regions) DrawingUtils.drawPolygon(p, g, true, parentColor)
      for (p <- curChild.regions) DrawingUtils.drawPolygon(p, g, true, childColor)
      curPoly.size match {
        case 0 =>
        case 1 => DrawingUtils.drawCircle(curPoly.head, 2f, g, editColor, true, 2)
        case 2 =>DrawingUtils.drawLine(curPoly.head, curPoly.tail.head, g, editColor,  2)
        case _ => DrawingUtils.drawPolygon(PolygonRegion(curPoly), g, true, editColor)
      }



    }, -100)


  }

}
