package drawing.library

import java.awt.Color
import scala.util.Random

object ColorOps {
  implicit  class ColorOps(c: java.awt.Color)  {
    def setRed(red: Int): Color = new Color(red, c.getBlue, c.getGreen, c.getAlpha)
    def setBlue(blue: Int): Color = new Color(c.getRed, blue, c.getGreen, c.getAlpha)
    def setGreen(green: Int): Color = new Color(c.getRed, c.getBlue, green, c.getAlpha)
    def setAlpha(alpha: Int): Color = new Color(c.getRed, c.getBlue, c.getGreen, alpha)
  }

  def randomColor(seed: Int): Color = {
    val r = new Random(seed)
    new Color(r.nextInt(256), r.nextInt(256), r.nextInt(256))
  }
}
