package utils.math.planar

//import java.lang.StackWalker

import sun.java2d.pipe.OutlineTextRenderer
import utils.math.{WithAlmostEquals, planar, _}

object SegmentPlanar {
  def apply(x1: Scalar, y1: Scalar, x2: Scalar, y2: Scalar): SegmentPlanar = new SegmentPlanar(V2(x1, y1), V2(x2, y2))

  def apply(s: Seq[Scalar]): SegmentPlanar = apply(s(0), s(1), s(2), s(3))

  implicit def toOE(sio: Option[SegmentToSegmentPlanarIntersection]): Option[Either[PointIntersection, SegmentIntersection]] = sio.map {
    case p@PointIntersection(_) => Left(p)
    case s@SegmentIntersection(_) => Right(s)
  }
}


case class SegmentPlanar(v1: V2, v2: V2) {
  def reverse: SegmentPlanar = SegmentPlanar(v2, v1)

  def sampleAt(fraction: Scalar): V2 = start + body * fraction

  /** begin|-------S_0------S_1-----------S_2----------|end */
  /** begin|-------S_0------S_1 ...... S_{n-1}-----------S_n----------|end */
  def sampleNTimesInside(times: Int): Iterator[V2] = for (i <- 0 until times iterator) yield sampleAt((i + 1d) / (times + 1))

  def containsSegment(other: SegmentPlanar): Boolean = contains(other.v1) && contains(other.v2)

  def center: V2 = (v1 + v2) * HALF
  /** calculate x(y) using corresponding line equation x = k_y * y + b_y */
  @deprecated def xFromY(y: Scalar): Option[Scalar] = kY.flatMap(k => bY.map(b => k * y + b))

  /** k_y coeff in x(y) line equation */
  @deprecated def kY: Option[Scalar] = {
    val divisor = v1.y - v2.y //todo maybe in different order
    Option.when(divisor != 0)((v1.x - v2.x) / divisor)
  }

  /** b_y coeff in x(y) line equation */
  @deprecated def bY: Option[Scalar] = kY.map(k => v2.x - k * v2.y)

  def kX: Option[Scalar] = {
    val divisor = v2.x - v1.x
    Option.when(divisor != 0)((v2.y - v1.y) / divisor)
  }
  /** y = k * x + b  => b = y - k * x */
  def bX: Option[Scalar] = kX.map(k => v1.y - k * v1.x)

  def yFromX(x: Scalar): Option[Scalar] = for (k <- kX; b <- bX) yield k * x + b

  def flip: SegmentPlanar = SegmentPlanar(v2, v1)

  def +(translation: V2): SegmentPlanar = SegmentPlanar(v1 + translation, v2 + translation)

  def -(translation: V2): SegmentPlanar = SegmentPlanar(v1 - translation, v2 - translation)

  def *(translation: V2): SegmentPlanar = SegmentPlanar(v1 * translation, v2 * translation)

  def /(translation: V2): SegmentPlanar = SegmentPlanar(v1 / translation, v2 / translation)

  def rotate(rotation: Scalar, point: V2 = V2.ZERO): SegmentPlanar = SegmentPlanar(v1.rotateAroundPoint(rotation, point), v2.rotateAroundPoint(rotation, point))

  def scale(scale: Scalar, point: V2 = V2.ZERO): SegmentPlanar = SegmentPlanar(v1.scaleAroundPoint(scale, point), v2.scaleAroundPoint(scale, point))

  def angleOX: AngleCCWPlanar = AngleCCWPlanar(v1 + V2.ox, v1, v2)

  def angleOY: AngleCCWPlanar = AngleCCWPlanar(v1 + V2.oy, v1, v2)

  def xProjSigned: Scalar = v2.x - v1.x

  def yProjSigned: Scalar = v2.y - v1.y

  def lengthSquared: Scalar = (v2 - v1).lengthSquared

  def length: Scalar = v1.distance(v2)

  def contains(v2: V2): Boolean = collinear(v2) && inBoudingBox(v2)

  def collinear(v: V2): Boolean = TrianglePlanar(v1, v2, v).degenerate

  def start: V2 = v1

  def end: V2 = v2

  def body: V2 = v2 - v1

  def line: LinePlanar = LinePlanar(v1, body)

  def tan: Option[Scalar] = line.tan

  def normal: UnitV2 = body.rotate(HALF_PI)

  def toLeftNormal: V2 = body.normalize.rotate90CCW

  def toRightNormal: V2 = body.normalize.rotate90CW

  def isVertical: Boolean = v1.x == v2.x

  def isHorizontal: Boolean = v1.y == v2.y

  def clothesPoint(point: V2): V2 = {
    val po = point - start
    val bodyNorm = body.normalize
    val proj = po ** bodyNorm
    if (proj ~< 0) start
    else if (proj ~> body.length) end
    else start + proj * bodyNorm
  }

  def distanceTo(point: V2): Scalar = {
    val po = point - start
    val bodyNorm = body.normalize
    val proj = po ** bodyNorm
    if (proj ~< 0) start.distance(point)
    else if (proj ~> body.length) end.distance(point)
    else (start + proj * bodyNorm).distance(point)
  }

  /*min(
    v1.distance(point),
    v2.distance(point),
    intersection(LinePlanar(point, normal)) match {
      case Some(SegmentIntersection(_)) => 0
      case Some(PointIntersection(p)) => if (contains(p)) p.distance(point) else BIG_NUMBER
      case None => BIG_NUMBER
    }
  )*/

  /** Project point onto segment and return position of projected point as 0 at segment start 1 at segment end */
  def projectionUnit(point: V2): Scalar = {
    val po = point - start
    val bodyNorm = body.normalize
    val proj = po ** bodyNorm
    proj / body.length
  }

  def receiveProjectionFrom(point: V2): Boolean = {
    val po = point - start
    val bodyNorm = body.normalize
    val proj = po ** bodyNorm
    (proj ~>= 0) && (proj ~<= body.length)
  }

  //    intersection(LinePlanar(point, normal)) match {
  //    case Some(SegmentIntersection(_)) => contains(point)
  //    case Some(PointIntersection(p)) => true
  //    case None =>false
  //  }

  def intersects(ot: LinePlanar): Boolean = {
    val toS = start - ot.origin
    val toE = end - ot.origin
    val a1 = ot.direction.angleClampedToPi(toE)
    val a2 = ot.direction.angleClampedToPi(toS)
    (a1 ~= 0) || (a2 ~= 0) || (a1 ~= PI) || (a2 ~= PI) || (math.signum(a1) != math.signum(a2))
  }

  def intersection(ot: LinePlanar): Option[SegmentToSegmentPlanarIntersection] = {
    val toS = start - ot.origin
    val toE = end - ot.origin
    val a1 = ot.direction.angleClampedToPi(toE)
    val a2 = ot.direction.angleClampedToPi(toS)
    if ((a1 ~= 0) || (a1 ~= PI)) {
      if ((a2 ~= 0) || (a2 ~= PI)) {
        if (start != end) Some(SegmentIntersection(SegmentPlanar(start, end)))
        else Some(PointIntersection(start))
      } else Some(PointIntersection(a1))
    } else if ((a2 ~= 0) || (a2 ~= PI)) Some(PointIntersection(end))
    else if (math.signum(a1) != math.signum(a2)) ot.intersection(line).map(PointIntersection)
    else None

  }

  def intersection(ot: RayPlanar): Option[SegmentToSegmentPlanarIntersection] = {
    val toS = start - ot.origin
    val toE = end - ot.origin
    val a1 = ot.direction.angleClampedToPi(toE)
    val a2 = ot.direction.angleClampedToPi(toS)
    //    println(a1, a2, toS, toE, this,  ot.line.intersection(line),  ot.line.intersection(line))
    if (((a1 ~> HALF_PI) || (a1 ~< -HALF_PI)) && ((a2 ~> HALF_PI) || (a1 ~< -HALF_PI))) None
    else if (a1 ~= 0)
      if (a2 ~= 0)
        if (start != end) Some(SegmentIntersection(SegmentPlanar(start, end)))
        else Some(PointIntersection(start))
      else if (a2 ~= PI) Some(SegmentIntersection(SegmentPlanar(ot.origin, start)))
      else Some(PointIntersection(start))
    else if (a2 ~= 0)
      if (a1 ~= PI) Some(SegmentIntersection(SegmentPlanar(ot.origin, end)))
      else Some(PointIntersection(end))
    else {
      ot.line.intersection(line).flatMap { p =>
        if ((p - ot.origin).sameDirection(ot.direction)) Some(PointIntersection(p))
        else None
      }
    }
  }


  def intersection(ot: SegmentPlanar): Option[SegmentToSegmentPlanarIntersection] = {
    //degenerate cases
    if (length == 0) {
      if (ot.contains(v1)) Some(PointIntersection(v1)) else None
    } else if (ot.length == 0) {
      if (contains(ot.v1)) Some(PointIntersection(ot.v1)) else None
    } else {
      //https://stackoverflow.com/a/565282
      val q = start
      val s = body
      val p = ot.start
      val r = ot.body

      //u = (q − p) × r / (r × s)
      //t = (q − p) × s / (r × s)

      if ((r det s) ~= 0f) {
        if (((q - p) det r) ~= 0f) {
          /** If r × s = 0 and (q − p) × r = 0, then the two lines are collinear.
            * r s  - collinear
            * t0 = (q − p) · r / (r · r)
            * t1 = (q + s − p) · r / (r · r) = t0 + s · r / (r · r)
            */
          var t0 = ((q - p) ** r) / (r ** r)
          var t1 = ((q + s - p) ** r) / (r ** r)
          if ((s ** r) ~< 0) {
            val tmp = t0
            t0 = t1
            t1 = tmp
          }
          //todo check
          if (t1 ~< 0) None
          else if (t1 ~= 0f) Some(PointIntersection(p))
          else if (t0 ~> 1) None
          else if (t0 ~= 1f) Some(PointIntersection(p + r))
          else {
            val l_ = p + r * Math.max(t0, 0)
            val r_ = p + r * Math.min(t1, 1)
            if (l_ ~= r_) Some(PointIntersection(l_))
            else Some(SegmentIntersection(SegmentPlanar(l_, r_)))
          }
        } else {
          //If r × s = 0 and (q − p) × r ≠ 0, then the two lines are parallel and non-intersecting.
          return None
        }
      } else {
        //u = (q − p) × r / (r × s)
        //t = (q − p) × s / (r × s)
        val u: Scalar = ((q - p) det r) / (r det s)
        val t: Scalar = ((q - p) det s) / (r det s)

        if ((0d ~<= u) && (u ~<= 1d) && (0d ~<= t) && (t ~<= 1d)) {
          return Some(PointIntersection(p + t * r))
        } else {
          return None
        }
      }
    }
  }


  def inBoudingBox(q: V2): Boolean =
    (q.x ~<= max(v1.x, v2.x)) && (q.x ~>= min(v1.x, v2.x)) &&
      (q.y ~<= max(v1.y, v2.y)) && (q.y ~>= min(v1.y, v2.y))

  def intersects(o: SegmentPlanar): Boolean = {
    val o1 = TrianglePlanar(v1, v2, o.v1).orientation
    val o2 = TrianglePlanar(v1, v2, o.v2).orientation
    val o3 = TrianglePlanar(o.v1, o.v2, v1).orientation
    val o4 = TrianglePlanar(o.v1, o.v2, v2).orientation

    if (o1 != o2 && o3 != o4) return true

    if (o1 == 0 && inBoudingBox(o.v1)) return true
    if (o2 == 0 && inBoudingBox(o.v2)) return true
    if (o3 == 0 && o.inBoudingBox(v1)) return true
    if (o4 == 0 && o.inBoudingBox(v2)) return true

    return false
  }

  private def onSegmentIfInLineWOEnds(q: V2): Boolean =
    q.x < Math.max(v1.x, v2.x) && q.x > Math.min(v1.x, v2.x) &&
      q.y < Math.max(v1.y, v2.y) && q.y > Math.min(v1.y, v2.y)

  def intersectsWOEnds(o: SegmentPlanar): Boolean = {
    val o1 = TrianglePlanar(v1, v2, o.v1).orientation
    val o2 = TrianglePlanar(v1, v2, o.v2).orientation
    val o3 = TrianglePlanar(o.v1, o.v2, v1).orientation
    val o4 = TrianglePlanar(o.v1, o.v2, v2).orientation
    //выстроились в линию
    if (o1 == 0 && o2 == 0 && o3 == 0 && o4 == 0) {
      return onSegmentIfInLineWOEnds(o.v1) || onSegmentIfInLineWOEnds(o.v2) || o.onSegmentIfInLineWOEnds(v1) || o.onSegmentIfInLineWOEnds(v2)
    }
    //касаются концами
    if (o1 == 0 || o2 == 0 || o3 == 0 || o4 == 0) return false
    //пересекаются
    return o1 != o2 && o3 != o4
  }

  def onSameLine(ot:SegmentPlanar):Boolean = line.contains(ot)
}

sealed trait SegmentToSegmentPlanarIntersection

case class PointIntersection(p: V2) extends SegmentToSegmentPlanarIntersection

case class SegmentIntersection(s: SegmentPlanar) extends SegmentToSegmentPlanarIntersection
