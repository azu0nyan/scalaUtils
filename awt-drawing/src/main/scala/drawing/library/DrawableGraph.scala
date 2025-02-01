package drawing.library

import java.awt.{Color, Graphics2D}

import drawing.core.SimpleDrawable
import utils.datastructures.graph.Graph.Graph
import utils.math.Scalar
import utils.math.planar.V2

class DrawableGraph[ND, ED](
                           var graph:Graph[ND, ED],
                           var nodeToLabel:Option[ND => String] =  None,
                           var nodeToPosition: ND => V2,
                           var edgeToWidth:ED => Scalar = (x:ED) => 1d,
                           val nodeRadius:Scalar,
                           var nodesColor:Color,
                           var edgesColor:Color,
                           var drawArrows:Boolean = false
                           ) extends SimpleDrawable{

  def drawNodes(g:Graphics2D):Unit =
    graph.nodes.foreach{ node =>
      val pos = nodeToPosition(node)
      DrawingUtils.drawCircle(pos, nodeRadius.toFloat, g, nodesColor, true)
      nodeToLabel.foreach{ ntl =>
        DrawingUtils.drawText(ntl(node), pos, g, 20d, true, Color.BLACK)
      }
    }

  def drawEdges(g:Graphics2D):Unit =
    graph.edgesAndNeighbours.foreach{ case (from, edge, to) =>
     if(drawArrows)DrawingUtils.drawArrow(nodeToPosition(from), nodeToPosition(to), g, edgesColor, DrawingUtils.camera.worldToScreen(edgeToWidth(edge)).toInt)
     DrawingUtils.drawLine(nodeToPosition(from), nodeToPosition(to), g, edgesColor, DrawingUtils.camera.worldToScreen(edgeToWidth(edge)).toInt)
    }

  override def drawAndUpdate(g: Graphics2D, dt: Scalar): Unit = {
    drawEdges(g)
    drawNodes(g)
  }
}
