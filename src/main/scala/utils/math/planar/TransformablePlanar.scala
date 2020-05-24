package utils.math.planar

import utils.math._
trait TransformablePlanar[TRANFORMED_TYPE] {

  def map(f: V2 => V2): TRANFORMED_TYPE

  def +(v:V2) :TRANFORMED_TYPE= map( _ + v)

  def -(v:V2) :TRANFORMED_TYPE = map( _ - v)

  def *(v:V2) :TRANFORMED_TYPE = map( _ * v)

  def /(v:V2) :TRANFORMED_TYPE = map( _ / v)

  def rotate(rotation: Scalar, point: V2 = V2.ZERO)  :TRANFORMED_TYPE = map(_.rotateAroundPoint(rotation, point))

  def scale(scale: Scalar, point: V2 = V2.ZERO) :TRANFORMED_TYPE = map(v => v.scaleAroundPoint(scale, point))
}
