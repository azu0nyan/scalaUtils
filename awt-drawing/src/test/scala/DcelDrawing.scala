import drawing.Drawing
import drawing.library.{DrawableDcel, DrawingUtils}
import utils.datastructures.dcel.PlanarDCEL
import utils.math.planar.{AngleOps, V2}
import utils.math._

import java.awt.Color
import java.awt.event.{MouseEvent, MouseListener}
import java.util.concurrent.atomic.AtomicInteger

object DcelDrawing extends App {
  Drawing.startDrawingThread()
  //FUCK YOU
  new Thread(()=> {
     Thread.sleep(100)
    Drawing.setSize(1920, 1079)
  }).start()

  Drawing.camera.invertY = true
  Drawing.FpsCounter.enable()
  Drawing.camera.enableControls()

  val dcel = new PlanarDCEL[V2, Int, Int](0, x => x)
  dcel.onNewFace.subscribe(f => println("new face", f.data, f.vertices.size, f.vertices.map(_.data).toSeq))
  dcel.onNewEdge.subscribe(e => println("new edge data", e.data, e.twin.data, dcel.pos(e.origin), dcel.pos(e.ending),
    "leftFace", e.leftFace.data, e.twin.leftFace.data, "pn tpn", e.prev.data, e.next.data, e.twin.prev.data, e.twin.next.data))
  dcel.onNewVertex.subscribe(v => println("new vertex", v.data, dcel.pos(v), v.edgesWithOriginHere.size))
  dcel.onEdgeCollapse.subscribe(e => println("edge collapse", e.data, e.twin.data, dcel.pos(e.origin), dcel.pos(e.ending), "leftFace", e.leftFace.data, e.twin.leftFace.data) )
  dcel.onEdgeSplit.subscribe{case (e1, e2) =>
    println("edge split", e1.data, e1.twin.data, dcel.pos(e1.origin), dcel.pos(e1.ending), "leftFace", e1.leftFace.data, e1.twin.leftFace.data)
    println("edge split", e2.data, e2.twin.data, dcel.pos(e2.origin), dcel.pos(e2.ending), "leftFace", e2.leftFace.data, e2.twin.leftFace.data)
  }

  val ids = new AtomicInteger()
  val ids2 = new AtomicInteger()

  def addPoly(s: Seq[V2]) = {
    println("Adding poly")
    dcel.cutPoly(s, x => x, (x, y) => (ids.getAndIncrement(), ids.getAndIncrement()), x => ids2.getAndIncrement())
    println("Added")
  }

  Drawing.addDrawer(g => {

    g.setColor(Color.BLACK)
    g.drawLine(200, 2000, 300, 100)
  })

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


  var poly:Seq[V2] = Seq()

  Drawing.addMouseLeftClickBinding(v => poly = poly :+ v)
  Drawing.addMouseRightClickBinding{ v =>
    addPoly(poly)
    poly = Seq()
  }


 Drawing.addDrawer{ g=>
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


    DrawingUtils.drawArrow(V2(100, 100), V2(100, 200),g = g,  color = Color.BLACK, arrowHeadSize = 10d)


  }

  //  dcel.
}
