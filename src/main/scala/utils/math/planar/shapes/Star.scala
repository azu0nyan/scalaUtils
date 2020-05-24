package utils.math.planar.shapes

import utils.math.planar.{Polygon, V2}
import utils.math._
import utils.math.planar.Polygon.Polygonable
import utils.math.planar.patch.Arc.Arc
import utils.math.planar.patch.BezierCurve.BezierCurveNPoints
import utils.math.planar.patch.Path.Path

object Star {

  case class SimpleStar(
                         center: V2 = V2.ZERO,
                         innerRadius: Scalar,
                         outerRadius: Scalar,
                         rays: Int
                       ) extends Polygonable {

    val innerCircle: Arc = Arc(center, innerRadius)

    val outerCircle: Arc = Arc(center, outerRadius)

    override def toPolygon: Polygon = Polygon.from(innerCircle.toPoints(rays * 2).zip(outerCircle.toPoints(rays * 2)).grouped(2).flatMap(gr => Seq(gr(0)._1, gr(1)._2)).toSeq)
  }


  case class FlatEndedStar(
                            center: V2 = V2.ZERO,
                            innerRadius: Scalar,
                            outerRadius: Scalar,
                            rays: Int,
                            outerFlatLength: Scalar = 0.5,
                            innerFlatLength: Scalar = 0.3,
                            sibdivsPerRay: Int = 10
                          ) extends Polygonable {

    override def toPolygon: Polygon = {
      val innerCircle: Arc = Arc(center, innerRadius)


      val outerCircle: Arc = Arc(center, outerRadius)

      val argFrame = TWO_PI / rays

      val halfArgFrame = argFrame / 2

      var res: Seq[V2] = Seq()
      for (i <- 0 until rays) {
        val currentArg = i * TWO_PI / rays
        val nextArg = currentArg + argFrame

        val start = innerCircle(currentArg)
        val top = outerCircle(currentArg + halfArgFrame)
        val end = innerCircle(nextArg)

        val startTangent = innerCircle.tangentAt(currentArg)
        val topTangent = outerCircle.tangentAt(currentArg + halfArgFrame)
        val endTangent = innerCircle.tangentAt(nextArg)

        val startCp = start + startTangent * innerFlatLength
        val topFirstCp = top - topTangent * outerFlatLength
        val midStartCp = (startCp + topFirstCp) * HALF
        val topSecondCp = top + topTangent * outerFlatLength
        val endCp = end - endTangent * innerFlatLength
        val midEndCp = (topSecondCp + endCp) * HALF

        res ++= Seq(start, startCp, midStartCp, topFirstCp, top, topSecondCp, midEndCp, endCp)
      }
      Polygon.from(res)
    }
  }


  case class CurvedRaysStar(
                         override val center: V2 = V2.ZERO,
                         override val innerRadius: Scalar,
                         override val outerRadius: Scalar,
                         override val rays: Int,
                         override val outerFlatLength: Scalar = 0.5,
                         override val innerFlatLength: Scalar = 0.3,
                         override val sibdivsPerRay: Int = 10
                       ) extends CustomStar {
    override def customRayShape(start: V2, startCp: V2, midStart: V2, topFirstCp: V2, top: V2, topSecondCp: V2, midEnd: V2, endCp: V2, end: V2, startTangent: V2, topTangent: V2, endTangent: V2): Seq[V2] =
      BezierCurveNPoints(IndexedSeq(start, top, end)).toPoints(sibdivsPerRay)
  }
  case class CurvedBetweenRaysStar(
                             override val center: V2 = V2.ZERO,
                             override val innerRadius: Scalar,
                             override val outerRadius: Scalar,
                             override val rays: Int,
                             override val outerFlatLength: Scalar = 0.5,
                             override val innerFlatLength: Scalar = 0.5,
                             override val sibdivsPerRay: Int = 10
                           ) extends CustomStar {
    override def customRayShape(start: V2, startCp: V2, midStart: V2, topFirstCp: V2, top: V2, topSecondCp: V2, midEnd: V2, endCp: V2, end: V2, startTangent: V2, topTangent: V2, endTangent: V2): Seq[V2] =
      BezierCurveNPoints(IndexedSeq(start, startCp, top)).toPoints(sibdivsPerRay) ++
      BezierCurveNPoints(IndexedSeq(top, endCp, end)).toPoints(sibdivsPerRay)
  }

  case class CurvedEverywhereStar(
                                    override val center: V2 = V2.ZERO,
                                    override val innerRadius: Scalar,
                                    override val outerRadius: Scalar,
                                    override val rays: Int,
                                    override val outerFlatLength: Scalar = 0.5,
                                    override val innerFlatLength: Scalar = 0.5,
                                    override val sibdivsPerRay: Int = 10
                                  ) extends CustomStar {
    override def customRayShape(start: V2, startCp: V2, midStart: V2, topFirstCp: V2, top: V2, topSecondCp: V2, midEnd: V2, endCp: V2, end: V2, startTangent: V2, topTangent: V2, endTangent: V2): Seq[V2] =
      BezierCurveNPoints(IndexedSeq(start, startCp, midStart)).toPoints(sibdivsPerRay) ++
       BezierCurveNPoints(IndexedSeq(midStart, topFirstCp, top)).toPoints(sibdivsPerRay) ++
      BezierCurveNPoints(IndexedSeq(top, topSecondCp, midEnd)).toPoints(sibdivsPerRay) ++
      BezierCurveNPoints(IndexedSeq(midEnd, endCp, end)).toPoints(sibdivsPerRay)
  }

  trait CustomStar extends Polygonable {    
    val center: V2 = V2.ZERO
    def innerRadius: Scalar
    def outerRadius: Scalar
    def rays: Int
    def outerFlatLength: Scalar = 0.5
    def innerFlatLength: Scalar = 0.3
    def sibdivsPerRay: Int = 10

    override def toPolygon: Polygon = {
      val innerCircle: Arc = Arc(center, innerRadius)


      val outerCircle: Arc = Arc(center, outerRadius)

      val argFrame = TWO_PI / rays

      val halfArgFrame = argFrame / 2

      var res: Seq[V2] = Seq()
      for (i <- 0 until rays) {
        val currentArg = i * TWO_PI / rays
        val nextArg = currentArg + argFrame

        val start = innerCircle(currentArg)
        val top = outerCircle(currentArg + halfArgFrame)
        val end = innerCircle(nextArg)

        val startTangent = innerCircle.tangentAt(currentArg)
        val topTangent = outerCircle.tangentAt(currentArg + halfArgFrame)
        val endTangent = innerCircle.tangentAt(nextArg)

        val startCp = start + startTangent * innerFlatLength
        val topFirstCp = top - topTangent * outerFlatLength
        val midStart = (startCp + topFirstCp) * HALF
        val topSecondCp = top + topTangent * outerFlatLength
        val endCp = end - endTangent * innerFlatLength
        val midEnd = (topSecondCp + endCp) * HALF
        println(start)
        res = res ++ customRayShape(start,startCp, midStart,topFirstCp,
          top, topSecondCp, midEnd, endCp, end,
          startTangent, topTangent, endTangent,
        )
          /*Seq(
            BezierCurve(IndexedSeq(start, top, end)),
            //  BezierCurve(IndexedSeq(start, startCp, midStart)),
            //   BezierCurve(IndexedSeq(midStart, topFirstCp, top)),
            //  BezierCurve(IndexedSeq(top, topFirstCp, midEnd)),
            //  BezierCurve(IndexedSeq(midEnd, endCp, end)),
          )*/


        //Seq(start, startCp, midStart, topFirstCp, top, topSecondCp, midEnd, endCp)
      }
      Polygon.from(res)
    }
    
    def customRayShape(start:V2,startCp:V2, midStart:V2,topFirstCp:V2, 
                       top:V2, topSecondCp:V2, midEnd:V2, endCp:V2, end:V2, 
                       startTangent:V2, topTangent:V2, endTangent:V2, 
                       ):Seq[V2] 
    
  }


}
