import drawing.Drawing
import drawing.library.DrawableDcel
import utils.datastructures.dcel.PlanarDCEL
import utils.math.planar.V2

object DcelDrawing extends App  {
  Drawing.startDrawingThread()
  val dcel = new PlanarDCEL[V2, Int, Int](0, x => x)
  Drawing.addDrawable(new DrawableDcel(dcel))
}
