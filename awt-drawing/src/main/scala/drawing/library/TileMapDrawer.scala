package drawing.library

import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D}

import drawing.core.SimpleDrawable
import utils.datastructures.IntV2
import utils.heightmap.Tilemap
import utils.math._

class TileMapDrawer(var h:Tilemap, var offset:IntV2 = IntV2(0, 0)) extends SimpleDrawable(){

  def draw(b:BufferedImage) : Unit = {
    val max = IntV2(Math.min(b.getHeight, h.resolution.i), Math.min(b.getWidth, h.resolution.j))
    for(i <- 0 until max.i;
      j <- 0 until max.j
    ) {
      val height = h.valueAt(IntV2(i, j))
      b.setRGB(i,j, getColor(height).getRGB)
    }
  }

  def getColor(height:Scalar):Color = {
    val dark = (utils.math.clamp(height, 0f, 1f) * 255).intValue()
    new Color(dark, dark, dark)
  }

  override def drawAndUpdate(g: Graphics2D, v: Double): Unit = {
    val img = new BufferedImage(h.resolution.i, h.resolution.j, BufferedImage.TYPE_INT_ARGB)
    draw(img)
    g.drawImage(img, offset.i, offset.j , null)
  }
}
