package utils.math.planar.patch

import utils.math._
import utils.math.planar.{PolygonalChain, SegmentPlanar, TransformablePlanar, V2}


object Path {
  /**
    *Result of searching closest point on spline
    * @param point closest point
    * @param argument curve(arg) = point
    * @param distance to point
    */
  case class SearchResult (point:V2, argument:Scalar, distance:Scalar)

  def uniformCombinePatches(s: Seq[Path]): Path = new Path {
    private val patches = s.map(_.IterpolateArgToUnit).toIndexedSeq
    override def posFromT(t: Scalar): V2 =  {
      if(t <= argStart) patches.head(0d)
      else if(t >= argEnd) patches.last(1d)
      else patches(t.toInt)(t - t.toInt)
    }

    override def argStart: Scalar = 0d

    override def argEnd: Scalar = s.length.toDouble
  }


  def toPoints(s: Seq[Path], pointsPerPath: Int, splitStraitPaths: Boolean = false): Seq[V2] = s.flatMap {
    case StraitPath(s) if !splitStraitPaths => Seq(s.start, s.end)
    case path@_ => path.toPoints(pointsPerPath)
  }


  trait Path extends TransformablePlanar[Path] {
    def IterpolateArgToUnit: Path = new Path {
      private def argInterpolation: Scalar => Scalar = lerp(0d, Path.this.argStart, 1d, Path.this.argEnd, _)
      override def posFromT(t: Scalar): V2 = Path.this.posFromT(argInterpolation(t))
      override def argStart: Scalar = 0d
      override def argEnd: Scalar = 1d
    }
    override def map(f: V2 => V2): Path = new Path {
      def posFromT(t: Scalar): V2 = f.apply(Path.this.apply(t))

      override def argStart: Scalar = Path.this.argStart

      override def argEnd: Scalar = Path.this.argEnd
    }

    def apply(arg: Scalar): V2 = posFromT(arg)

    def posFromT(t: Scalar): V2

    def argStart: Scalar

    def argEnd: Scalar

    /* point count = segmentCount + 1*/
    def argSubdivisions(segmentsCount: Int): IndexedSeq[Scalar] = (0 to segmentsCount).map(i => (i.toDouble / segmentsCount) * (argEnd - argStart) + argStart)

    def toPoints(pointCount: Int): IndexedSeq[V2] = argSubdivisions(pointCount - 1).map(apply)

    def toPolygonalChain(segmentCount: Int, closed: Boolean = false): PolygonalChain = PolygonalChain(argSubdivisions(segmentCount).map(apply), closed)
    //PointArgDistance

    def closestPointArgBinarySearch(point: V2, maxDepth: Int = 10, initialSubdivTries: Int = 0): SearchResult = {
      var (closestArg, subdivSize, closestDistance): (Scalar, Scalar, Scalar) =
        if (initialSubdivTries == 0)
          ((argEnd - argStart) / 2d, (argEnd - argStart) / 2d, this ((argEnd - argStart) / 2d).distance(point)) else {
          val subdivSize_ = (argEnd - argStart) / (initialSubdivTries + 1)
          var closestArg_ = subdivSize_
          var closestDistance_ = this (closestArg_).distance(point)
          for (i <- 1 until (initialSubdivTries + 1)) {
            val arg = argStart + i * subdivSize_
            val dist = this (arg).distance(point)
            if (dist < closestDistance_) {
              closestArg_ = arg
              closestDistance_ = dist
            }
          }
          (closestArg_, subdivSize_, closestDistance_)
        }
      var depthLeft = maxDepth
      while ((closestDistance !~= 0d) && depthLeft != 0) {
        depthLeft -= 1
        subdivSize /= 2 // ??
        val lArg = closestArg - subdivSize
        val lDist = this (lArg).distance(point)
        val rArg = closestArg + subdivSize
        val rDist = this (rArg).distance(point)
        //if the distance to t ± interval/2 is larger than the distance to t, try again with the interval reduced to half its original length.
        if (lDist > closestDistance && rDist > closestDistance) {

        } else {
          if (lDist < closestDistance) {
            closestDistance = lDist
            closestArg = lArg
          }
          if (rDist < closestDistance) {
            closestDistance = rDist
            closestArg = rArg
          }
        }
        // val rArg = closestArg + subdivSize
      }
      SearchResult(this (closestArg), closestArg, closestDistance)
    }

  }


  trait PathWithTangent extends Path {
    /*correctly override*/
    def tangentAt(arg: Scalar): V2 = ((apply(arg) - apply(arg - 0.001)) + (apply(arg + 0.001) - apply(arg))).normalize
  }

  case class StraitPath(s: SegmentPlanar) extends PathWithTangent {
    override def posFromT(t: _root_.utils.math.Scalar): V2 = s.v1 * (1 - t) + s.v2 * t

    override def argStart: _root_.utils.math.Scalar = 0

    override def argEnd: _root_.utils.math.Scalar = 1
  }

}


