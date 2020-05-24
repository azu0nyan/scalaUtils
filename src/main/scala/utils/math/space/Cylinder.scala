package utils.math.space

import utils.math.Scalar
import utils.math._

case class Cylinder(bot:V3, top:V3, radius:Scalar) {

  def normal:V3 = (top - bot).normalize

  def topPlane:Plane = Plane.fromPointAndNormal(top, normal)

  /**  has inverted normal  */
  def botPlane:Plane = Plane.fromPointAndNormal(bot, -normal)

  def height:Scalar = (top - bot).length

  def volume:Scalar = PI * radius * radius * height
}
