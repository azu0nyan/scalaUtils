package utils.math.planar.patch

import utils.math.planar.{Polygon, V2}
import utils.math.planar.patch.Path.PathWithTangent
import utils.math._
import utils.math.planar.patch.Arc.ArcTrait


object Arc {

  trait ArcTrait extends PathWithTangent {
    def center: V2

    def radius: Scalar

    def angleStart: Scalar

    def angleEnd: Scalar

    override def tangentAt(arg: Scalar): V2 = (posFromT(arg) - center).rotate90CW.normalize

    override def argStart: Scalar = angleStart

    override def argEnd: Scalar = angleEnd

    override def posFromT(t: Scalar): V2 = center + V2.ox.rotate(t) * radius
  }

  case class Circle(center:V2, radius:Scalar) extends ArcTrait {
    override def angleStart:Scalar = 0

    override def angleEnd: Scalar = TWO_PI

  }

  case class Arc(center: V2, radius: Scalar, angleStart: Scalar = 0d, angleEnd: Scalar = TWO_PI) extends ArcTrait {


  }

}




