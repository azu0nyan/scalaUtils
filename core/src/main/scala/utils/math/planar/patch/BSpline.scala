package utils.math.planar.patch

import utils.math.*
import utils.math.linalg.{LinearSystemSolver, Matrix3x3}
import utils.math.planar.V2
import utils.math.planar.patch.BezierCurve.{BezierCurve, BezierCurve2, BezierCurve3, BezierCurveNPoints, BezierCurveWrongNumberOfPoints}
import utils.math.planar.patch.Path.{Path, PathWithTangent, SearchResult}
import utils.math.space.V3

object BSpline {

  implicit def toBSpline(c: BezierCurve): BSpline = BSpline(IndexedSeq(c))
  implicit def toBSpline(c: Seq[BezierCurve]): BSpline = BSpline(c.toIndexedSeq)

  /** Relaxed uniform cubic B-spline */
  def fromControlPolygon(b: Seq[V2]): BSpline = {
    val n = b.size - 1
    val s: Array[V2] = new Array[V2](n + 1)
    s(0) = b(0)
    s(n) = b(n)
    for (i <- 1 to (n - 1)) {
      s(i) = b(i - 1) * (1.0 / 6.0) + b(i) * (2.0 / 3.0) + b(i + 1) * (1.0 / 6.0)
    }

    for (i <- 1 to n) yield BezierCurve3(
      s(i - 1),
      b(i - 1) * (2.0 / 3.0) + b(i) * (1.0 / 3.0),
      b(i - 1) * (1.0 / 3.0) + b(i) * (2.0 / 3.0),
      s(i)
    )
  }

  private val threePointsEq: Matrix3x3 = Matrix3x3(
    2, 0, -1,
    0, 2, -1,
    .5d, .5d, 0
  ).inverseUnsafe

  def interpolateWithPoints(points: Seq[V2]): BSpline = points.size match {
    case 0 | 1 => throw BezierCurveWrongNumberOfPoints()
    case 2 => Seq(BezierCurveNPoints(points.toIndexedSeq))
    case 3 =>
      val s1s2AX: V3 = threePointsEq * V3(points(0).x, points(2).x, points(1).x)
      val s1s2AY: V3 = threePointsEq * V3(points(0).y, points(2).y, points(1).y)
      val s1 = V2(s1s2AX(0), s1s2AY(0))
      val s2 = V2(s1s2AX(1), s1s2AY(1))
      //      val a = V2(s1s2AX(2), s1s2AX(2))
      Seq(BezierCurve2(points(0), s1, points(1)), BezierCurve2(points(1), s2, points(2)))
    case _ =>
      val n = points.size - 1 // n == 5 indices 0,1,2,3,4,5
      val s = points
      //    val b: Array[V2] = new Array[V2](n)
      //    b(0) = s(0)
      //    b(n) = s(n)

      val sX: Seq[Scalar] = (6d * s(1).x - s(0).x) +: s.slice(2, n - 1).map(_.x * 6d) :+ (6d * s(n - 1).x - s(n).x)
      val sY: Seq[Scalar] = (6d * s(1).y - s(0).y) +: s.slice(2, n - 1).map(_.y * 6d) :+ (6d * s(n - 1).y - s(n).y)

      val tria: IndexedSeq[Scalar] = for (i <- 1 to (n - 1)) yield 1d
      val trib: IndexedSeq[Scalar] = for (i <- 1 to (n - 1)) yield 4d
      val tric: IndexedSeq[Scalar] = for (i <- 1 to (n - 1)) yield 1d


      val bX = LinearSystemSolver.solveTridiagonal(tria, trib, tric, sX.toIndexedSeq)
      val bY = LinearSystemSolver.solveTridiagonal(tria, trib, tric, sY.toIndexedSeq)

      val b = s(0) +: (bX.zip(bY).map { case (x, y) => V2(x, y) }) :+ s(n)


      for (i <- 1 to n) yield BezierCurve3(
        s(i - 1),
        b(i - 1) * (2.0 / 3.0) + b(i) * (1.0 / 3.0),
        b(i - 1) * (1.0 / 3.0) + b(i) * (2.0 / 3.0),
        s(i)
      )
  }

}


case class BSpline(splines: IndexedSeq[BezierCurve]) extends PathWithTangent {
  override def posFromT(t: Scalar): V2 = {
    if (t <= argStart) splines.head(0d)
    else if (t >= argEnd) splines.last(1d)
    else splines(t.toInt)(t - t.toInt)
  }

  override def argStart: Scalar = 0d
  override def argEnd: Scalar = splines.size
  /** check all curves at few positions in spline trying to find closest, then doing fine search
    * can ne optimized for less memory allocations
    * */
  override def closestPointArgBinarySearch(point: V2, maxDepth: Int, initialSubdivTries: Int): SearchResult = {
    val sr = splines.map { s => (s, s.toPoints(5).map(_.distance(point)).min) }.sortBy(_._2).take(3)
      .map{case (s, _) => (s, s.closestPointArgBinarySearch(point, maxDepth, initialSubdivTries))}.minBy(_._2.distance)
    sr._2.copy(argument = sr._2.argument + splines.indexOf(sr._1))

  }
  override def tangentAt(arg: Scalar): V2 = tangentAtApproximation(arg) //todo correct
}


