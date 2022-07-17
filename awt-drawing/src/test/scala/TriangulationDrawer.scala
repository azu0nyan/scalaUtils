import drawing.Drawing
import drawing.core.DrawingWindow
import drawing.library.{DrawableGrid, DrawingUtils}
import utils.datastructures.IntV2
import utils.math.planar.algo.{PolygonContains, PolygonTriangulation}
import utils.math.planar.{Polygon, PolygonRegion, V2}

import java.awt.Font
import java.awt.Color
import java.awt.event.KeyEvent
import java.io.{File, FileOutputStream, PrintWriter}
import java.util.logging.{Level, Logger}
import scala.util.Using


object TriangulationDrawer {
  Logger.getLogger("UTILS").setLevel(Level.SEVERE)
  var poly: Polygon = Polygon(Seq())
  var building: Seq[V2] = List(V2(-213.0, 414.0), V2(-273.0, 353.0), V2(-358.0, 361.0), V2(-393.0, 310.0), V2(-389.0, 254.0), V2(-343.0, 230.0), V2(-170.0, 249.0), V2(-87.0, 116.0), V2(-233.0, 41.0), V2(-157.0, -67.0), V2(93.0, 14.0))


  def dump(addStr: Option[String] = None): Unit = {
    println(s"Dumping current polys id=$dumpID")
    Using(new PrintWriter(new FileOutputStream(new File("TrPolys.txt"), true))) { pw =>
      pw.println(s"-------------$dumpID-------------")
      pw.println(s"setPoly($poly)")
      pw.println(s"var building: Seq[V2] = $building")
      for(s <- addStr)pw.println(addStr)
      dumpID += 1
    }
  }
  var monotonePartition: Either[Throwable, Seq[Seq[V2]]] = Right(Seq())
  def setPoly(p:Polygon): Unit = {
    poly = p
    try{
      monotonePartition = Right(PolygonTriangulation.monotonePartition(poly.regions.map(_.vertices)))
    }catch {
      case t:Throwable =>
        t.printStackTrace()
        dump(Some(t.getStackTrace.map(_.toString).mkString("\n")))
        monotonePartition = Left(t)
    }
  }


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
    val editColor = new Color(255, 0, 0, 127)
    var snapValue = 100

    Drawing.addDrawable(new DrawableGrid(IntV2(-2000, -2000), IntV2(2000, 2000), lineEvery = snapValue), -1000)


    def addPolyClick(): Unit = {
      println("Add poly")
      if (building.nonEmpty) {
        setPoly(poly.addRegion(PolygonRegion(building)))
        building = Seq()
      }
    }

    Drawing.addKeyBinding(KeyEvent.VK_ENTER, addPolyClick)
    Drawing.addKeyBinding(KeyEvent.VK_SPACE, addPolyClick)
    Drawing.addKeyBinding(KeyEvent.VK_Z,
      if (Drawing.shiftControlAlt.controlPressed && !Drawing.shiftControlAlt.shiftPressed) {
        building = building.dropRight(1)
      } else if (Drawing.shiftControlAlt.controlPressed && Drawing.shiftControlAlt.shiftPressed) {
        building = building.drop(1)
      }
    )



    Drawing.addKeyBinding(KeyEvent.VK_BACK_SPACE, {
      setPoly(Polygon(poly.regions.reverse.tail.reverse))
    })


    Drawing.addKeyBinding(KeyEvent.VK_0, {
      dump()
    }, true)

    def modPosition(pos: V2): V2 = {
      if (Drawing.shiftControlAlt.shiftPressed) {
        val x = Math.floor((pos + V2(snapValue / 2)).x / snapValue) * snapValue
        val y = Math.floor((pos + V2(snapValue / 2)).y / snapValue) * snapValue
        V2(x, y)
      } else if (Drawing.shiftControlAlt.controlPressed) {
        (poly.vertices).minByOption(_.distance(pos)).getOrElse(pos)
      } else if (Drawing.shiftControlAlt.altPressed) {
        (poly.sides ).map(_.clothesPoint(pos)).minByOption(_.distance(pos)).getOrElse(pos)
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

      building = building :+ point
    })

    Drawing.addMouseRightClickBinding { pos => {
        poly.regions.find(_.contains(pos)) match {
          case Some(reg) =>
            poly = poly.copy(regions = poly.regions.filter(_ != reg) ++ Option.when(building.size >= 3)(PolygonRegion(building)).toSeq)
            building = reg.vertices
          case None =>
        }
    }}

    Drawing.addDrawer(g => {
      for (p <- poly.regions) DrawingUtils.drawPolygon(p, g, true, parentColor)
      for (p <- poly.regions) DrawingUtils.drawPolygon(p, g, false, Color.RED, 1)
      building.size match {
        case 0 =>
        case 1 => DrawingUtils.drawCircle(building.head, 2f, g, editColor, true, 2)
        case 2 => DrawingUtils.drawLine(building.head, building.tail.head, g, editColor, 2)
        case _ => DrawingUtils.drawPolygon(PolygonRegion(building), g, true, editColor)
      }
      monotonePartition match {
        case Left(value) => DrawingUtils.drawText(value.toString, V2(-100, 100), g, 20,  false)
        case Right(m) =>
          for (p <- m) DrawingUtils.drawPolygon(PolygonRegion(p), g, false, Color.BLACK, 3)
      }



    }, -100)


  }

}
