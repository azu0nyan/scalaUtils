package utils.math.space

import utils.math.planar.{QuadPlanar, Rectangle}
import utils.math._
object Cubuoid{
  def apply(a:AABox): Cubuoid = new Cubuoid(
    a.angles(0),
    a.angles(1),
    a.angles(2),
    a.angles(3),
    a.angles(4),
    a.angles(5),
    a.angles(6),
    a.angles(7)
  )
  def apply(o:OBox): Cubuoid = new Cubuoid(
    o.angles(0),
    o.angles(1),
    o.angles(2),
    o.angles(3),
    o.angles(4),
    o.angles(5),
    o.angles(6),
    o.angles(7)
  )
}

//@formatter:off
/**
  *     v7________ v8
  *    /|          /|
  *   / |         / |
  * v5__|________v6 |
  * |   |        |  |
  * |   v3_______| v4
  * |  /         | /
  * | /          |/
  * v1___________v2
  *
  *///@formatter:on
case class Cubuoid(
                           v1: V3,
                           v2: V3,
                           v3: V3,
                           v4: V3,
                           v5: V3,
                           v6: V3,
                           v7: V3,
                           v8: V3
                         ) {
  def this(v: (V3, V3, V3, V3, V3, V3, V3, V3)) = {
    this(v._1, v._2, v._3, v._4, v._5, v._6, v._7, v._8)
  }

  /** same normal
    *
    * @param bot
    * @param top
    */
  def this(bot: Quad, top: Quad) = {
    this(bot.bl, bot.br, bot.tl, bot.tr, top.bl, top.br, top.tl, top.tr)
  }

  def this(perimeter: QuadPlanar, bot: Scalar, top: Scalar) = {
    this(perimeter.toQuad3(bot), perimeter.toQuad3(top))
  }



  def bot: Quad = new Quad(v1, v2, v3, v4).flipVertical

  def top = new Quad(v5, v6, v7, v8)

  def front = new Quad(v1, v2, v5, v6)

  def back = new Quad(v4, v3, v8, v7)

  def right = new Quad(v2, v4, v6, v8)

  def left = new Quad(v3, v1, v7, v5)

  def sides = List(bot, top, front, back, right, left)

}


class Parallelepiped(val min: V3, val max: V3) extends Cubuoid(new Rectangle(min.toPlanar, max.toPlanar), min.heightCord, max.heightCord){
  def this(size: V3) = this(  -size  * HALF,  size * HALF)
  def this(aaBox: AABox)= this(aaBox.min, aaBox.max)
}

class Cube(val size: Scalar) extends Parallelepiped(size)
