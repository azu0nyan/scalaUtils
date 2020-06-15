package drawing.library

import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D}

import drawing.Drawing
import drawing.core.{Camera, SimpleDrawable}
import utils.datastructures.IntV2
import utils.heightmap.HeightGrid
import utils.math._
import utils.math.planar.V2

/**
 * @param heightGrid heightGrid to draw
 * @param offset in world coordinates
 */
class HeightGridDrawer(
                        var heightGrid:HeightGrid,
                        var offset:V2 = V2(0, 0),
                        var scale:V2 = V2(1,1 ),
                        var heightToColor:Scalar => Color,
                        var onScreenLt:IntV2 = IntV2(0,0),
                        var onScreenRb:IntV2 = IntV2(1920,1080)
                      ) extends SimpleDrawable(){
  def camera:Camera = Drawing.camera

  def areaOnScreen(lt:IntV2, rb: IntV2):(IntV2, IntV2) = {
    val leftTopWorld = offset
    val rightBotWorld = offset +  heightGrid.resolution.toV2 * scale
    val leftTopScreen = IntV2.clamp(camera.worldToScreen(leftTopWorld).toIntV2, lt, rb)
    val rightBotScreen = IntV2.clamp(camera.worldToScreen(rightBotWorld).toIntV2, lt, rb)
//    leftTopWorld = camera.screenToWorld(leftTopScreen.toV2)
//    rightBotWorld = camera.screenToWorld(rightBotScreen.toV2)
    (leftTopScreen, rightBotScreen)
  }

  def generateImage(leftTopScreen:IntV2, rightBotScreen:IntV2) :BufferedImage = {
    val w = rightBotScreen.x - leftTopScreen.x
    val h = rightBotScreen.y - leftTopScreen.y
    val img = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    var i = 0
    while (i < w){
      var j = 0
      while (j < h){
        val onScreen = V2(i + leftTopScreen.x, j + leftTopScreen.y)
        val inWorld = camera.screenToWorld(onScreen)
        val inGrid = ((inWorld - offset) / scale).toIntV2
        val color = heightToColor(heightGrid.valueAt(inGrid))
        img.setRGB(i, j, color.getRGB)
        j +=1
      }
      i += 1
    }
    img

  }

  override def drawAndUpdate(g: Graphics2D, dt: Scalar): Unit = {
    val aos = areaOnScreen(onScreenLt, onScreenRb)
    if(onScreenLt.x != onScreenRb.x && onScreenLt.y != onScreenRb.y ) {
      val img = generateImage(aos._1, aos._2)
      g.drawImage(img, onScreenLt.x, onScreenLt.y, null)
    }
  }


//  def draw(b:BufferedImage) : Unit = {
//    val max = IntV2(Math.min(b.getHeight, h.resolution.i), Math.min(b.getWidth, h.resolution.j))
//    for(i <- 0 until max.i;
//      j <- 0 until max.j
//    ) {
//      val height = h.valueAt(IntV2(i, j))
//      b.setRGB(i,j, getColor(height).getRGB)
//    }
//  }
//
//  def getColor(height:Scalar):Color = {
//    val dark = (utils.math.clamp(height, 0f, 1f) * 255).intValue()
//    new Color(dark, dark, dark)
//  }
//
//  override def drawAndUpdate(g: Graphics2D, v: Double): Unit = {
//    val img = new BufferedImage(h.resolution.i, h.resolution.j, BufferedImage.TYPE_INT_ARGB)
//    draw(img)
//    g.drawImage(img, offset.i, offset.j , null)
//  }


}
