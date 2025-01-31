package utils.color

import utils.math.*

import java.awt

object Color{

  implicit def fromAwt(c:java.awt.Color):Color = new Color(c.getRed / 255.0f, c.getGreen / 255.0f, c.getBlue / 255.0f, c.getAlpha / 255.0f)//int getters
  implicit def toAwt(c:Color):java.awt.Color = new java.awt.Color(c.r.toFloat , c.g.toFloat, c.b.toFloat, c.a.toFloat)//float constructor
}

case class Color(r:Scalar, g:Scalar, b:Scalar, a:Scalar = 1f) {

}
