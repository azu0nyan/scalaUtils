package utils.datastructures

import utils.math.planar.{TrianglePlanar, V2}
import utils.math.space
import utils.math.space.{Triangle, V3}


object IndexedTriangle {
  implicit def toTriple(tr: IndexedTriangle): (Int, Int, Int) = tr.indices
}

case class IndexedTriangle(indices: (Int, Int, Int), f: Int => V3) {

  lazy val triangle: Triangle = space.Triangle(f(indices._1), f(indices._2), f(indices._3))

  lazy val flip:IndexedTriangle = IndexedTriangle((indices._1, indices._3, indices._2), f)

  def flipToMatchNormal(otherNormal: V3): IndexedTriangle = if (triangle.onPositiveSide(otherNormal)) this else flip

}
case class IndexedTriangle2(indices: (Int, Int, Int), f: Int => V2) {

  lazy val triangle: TrianglePlanar = TrianglePlanar(f(indices._1), f(indices._2), f(indices._3))

  lazy val flip:IndexedTriangle2 = IndexedTriangle2((indices._1, indices._3, indices._2), f)

//  def flipToMatchNormal(otherNormal: V3): IndexedTriangle = if (triangle.onPositiveSide(otherNormal)) this else flip

}