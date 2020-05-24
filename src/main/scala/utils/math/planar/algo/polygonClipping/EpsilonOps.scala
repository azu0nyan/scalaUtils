package utils.math.planar.algo.polygonClipping

import utils.math.planar.V2
import utils.math._

object EpsilonOps {
  //
  // provides the raw computation functions that takes epsilon into account
  //
  // zero is defined to be between (-epsilon, epsilon) exclusive
  //
 // val epsilon: Scalar = SMALL_NUMBER

  def pointAboveOrOnLine(pt: V2, left: V2, right: V2)(implicit  epsilon: Epsilon): Boolean = {
    val Ax = left.x
    val Ay = left.y
    val Bx = right.x
    val By = right.y
    val Cx = pt.x
    val Cy = pt.y
    return (Bx - Ax) * (Cy - Ay) - (By - Ay) * (Cx - Ax) >= -epsilon
  }

  def pointBetween(p: V2, left: V2, right: V2)(implicit  epsilon: Epsilon): Boolean = {
    // p must be collinear with left->right
    // returns false if p == left, p == right, or left == right
    val d_py_ly = p.y - left.y
    val d_rx_lx = right.x - left.x
    val d_px_lx = p.x - left.x
    val d_ry_ly = right.y - left.y

    val dot = d_px_lx * d_rx_lx + d_py_ly * d_ry_ly
    // if `dot` is 0, then `p` == `left` or `left` == `right` (reject)
    // if `dot` is less than 0, then `p` is to the left of `left` (reject)
    if (dot < epsilon)
      return false

    val sqlen = d_rx_lx * d_rx_lx + d_ry_ly * d_ry_ly
    // if `dot` > `sqlen`, then `p` is to the right of `right` (reject)
    // therefore, if `dot - sqlen` is greater than 0, then `p` is to the right of `right` (reject)
    if (dot - sqlen > -epsilon)
      return false

    return true
  }


  def pointsSameX(p1: V2, p2: V2)(implicit  epsilon: Epsilon): Boolean = return abs(p1.x - p2.x) < epsilon

  def pointsSameY(p1: V2, p2: V2)(implicit  epsilon: Epsilon): Boolean = return abs(p1.y - p2.y) < epsilon

  def pointsSame(p1: V2, p2: V2)(implicit  epsilon: Epsilon): Boolean = {
    return pointsSameX(p1, p2) && pointsSameY(p1, p2)
  }

  def pointsCompare(p1: V2, p2: V2)(implicit  epsilon: Epsilon): Int = {
    // returns -1 if p1 is smaller, 1 if p2 is smaller, 0 if equal
    if (pointsSameX(p1, p2))
      return if (pointsSameY(p1, p2)) 0 else if (p1.y < p2.y) -1 else 1
    return if (p1.x < p2.x) -1 else 1
  }

  def pointsCollinear(pt1: V2, pt2: V2, pt3: V2)(implicit  epsilon: Epsilon): Boolean = {
    // does pt1->pt2->pt3 make a straight line?
    // essentially this is just checking to see if the slope(pt1->pt2) === slope(pt2->pt3)
    // if slopes are equal, then they must be collinear, because they share pt2
    val dx1 = pt1.x - pt2.x
    val dy1 = pt1.y - pt2.y
    val dx2 = pt2.x - pt3.x
    val dy2 = pt2.y - pt3.y
    return abs(dx1 * dy2 - dx2 * dy1) < epsilon
  }


  class LineIntersectsResult(var pt: V2, var alongA: Int = 0, var alongB: Int = 0)

  def linesIntersect(a0: V2, a1: V2, b0: V2, b1: V2)(implicit  epsilon: Epsilon): Option[LineIntersectsResult] = {
    // returns None if the lines are coincident (e.g., parallel or on top of each other)
    //
    // returns an object if the lines intersect:
    //   {
    //     pt: [x, y],    where the intersection point is at
    //     alongA: where intersection point is along A,
    //     alongB: where intersection point is along B
    //   }
    //
    //  alongA and alongB will each be one of: -2, -1, 0, 1, 2
    //
    //  with the following meaning:
    //
    //    -2   intersection point is before segment's first point
    //    -1   intersection point is directly on segment's first point
    //     0   intersection point is between segment's first and second points (exclusive)
    //     1   intersection point is directly on segment's second point
    //     2   intersection point is after segment's second point
    val adx = a1.x - a0.x
    val ady = a1.y - a0.y
    val bdx = b1.x - b0.x
    val bdy = b1.y - b0.y

    val axb = adx * bdy - ady * bdx
    if (Math.abs(axb) < epsilon)
      return None // lines are coincident

    val dx = a0.x - b0.x
    val dy = a0.y - b0.y

    val A = (bdx * dy - bdy * dx) / axb
    val B = (adx * dy - ady * dx) / axb

    val res = new LineIntersectsResult(V2(a0.x + A * adx, a0.y + A * ady))

    // categorize where intersection point is along A and B

    if (A <= -epsilon)
      res.alongA = -2
    else if (A < epsilon)
      res.alongA = -1
    else if (A - 1 <= -epsilon)
      res.alongA = 0
    else if (A - 1 < epsilon)
      res.alongA = 1
    else
      res.alongA = 2

    if (B <= -epsilon)
      res.alongB = -2
    else if (B < epsilon)
      res.alongB = -1
    else if (B - 1 <= -epsilon)
      res.alongB = 0
    else if (B - 1 < epsilon)
      res.alongB = 1
    else
      res.alongB = 2

    return Some(res)
  }

  def pointInsideRegion(pt: V2, region: Region)(implicit  epsilon: Epsilon): Boolean = {
    val x = pt.x
    val y = pt.y
    var last_x = region.last.x
    var last_y = region.last.y
    var inside = false
    for (i <- region.indices) {
      val curr_x = region(i).x
      val curr_y = region(i).y

      // if y is between curr_y and last_y, and
      // x is to the right of the boundary created by the line
      if ((curr_y - y > epsilon) != (last_y - y > epsilon) &&
        (last_x - curr_x) * (y - curr_y) / (last_y - curr_y) + curr_x - x > epsilon)
        inside = !inside

      last_x = curr_x
      last_y = curr_y
    }
    return inside
  }
}


