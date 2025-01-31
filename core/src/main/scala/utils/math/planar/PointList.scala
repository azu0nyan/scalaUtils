package utils.math.planar

import utils.datastructures.CircullarOps
import utils.datastructures.spatial.AARectangle
import utils.math.*

object PointList{
  trait PointListOps[MYTYPE<: PointListOps[_]] extends TransformablePlanar[MYTYPE] {

    def replacePoints(vertices:Seq[V2]):MYTYPE

    def vertices:Seq[V2]

    def average: V2 = vertices.reduceOption((a: V2, b: V2) => a + b).getOrElse(V2.ZERO) / V2(Math.max(vertices.length, 1))

    def aabb: AARectangle = new AARectangle(
      V2(
        vertices.map(v => v.x).min,
        vertices.map(v => v.y).min,
      ),
      V2(
        vertices.map(v => v.x).max,
        vertices.map(v => v.y).max,
      )
    )

    def verticesCount: Int = vertices.length

    def indexOf(v: V2): Int = vertices.indexOf(v)

    /**
      *
      * @param myIndex i
      * @param otherIndex j
      * @param returnBackToCut if true //ex:combining polygons
      *                        O[0, i] + I[j, m] + I[0, m] + O[i, n]
      *                        else //ex:combining paths traversals
      *                        O[0, i] + I[j, m] + I[0, m) + O(i, n]
      */
    def mergeVerticesWithAt(myIndex: Int, otherIndex: Int, returnBackToCut:Boolean, ot:Seq[V2]): MYTYPE = {
      val myParts = vertices.splitAt(myIndex)
      val other = ot.splitAt(otherIndex)
      //
      val p1 = myParts._1 :+ myParts._2.head
      val p2 = other._2
      val p3 = other._1 :+ other._2.head
      val p4 = myParts._2
      replacePoints(p1 ++ p2 ++ p3 ++ p4)
    }

    def vertex(index:Int):V2 = vertices(CircullarOps.circullarIndex(index, verticesCount))

    def flipHorizontal(axis:Scalar = average.x) :MYTYPE = replacePoints(vertices.map(v => (v - V2(axis, 0d)) * V2(-1d, 1d) + V2(axis, 0d)).reverse)

    def flipVertical(axis:Scalar = average.y) :MYTYPE = replacePoints(vertices.map(v =>(v - V2( 0d, axis)) * V2(1d, -1d) + V2(0d, axis)).reverse)

    def clampPoints(box:AARectangle) :MYTYPE = map(v => clamp(v, box.min, box.max))

    def replaceXCord(toReplace:Scalar, replacement:Scalar):MYTYPE = map(v => if(v.x ~= toReplace) V2(replacement, v.y) else v)

    def replaceYCord(toReplace:Scalar, replacement:Scalar):MYTYPE = map(v => if(v.y ~= toReplace) V2(v.x, replacement) else v)

    def removeVertex(v: V2): MYTYPE = replacePoints(vertices.filter(o => o != v))

    def removeVertex(index: Int): MYTYPE = replacePoints(vertices.patch(index, Seq(), 1))

    def closestVertex(point:V2): V2 = vertices.minBy(v=> v.distance(point))

    def reverse:MYTYPE = replacePoints(vertices.reverse)

    override def map(f: V2 => V2): MYTYPE = replacePoints(vertices.map(f))
  }

  case class PointList(vertices:Seq[V2]) extends PointListOps[PointList] {

    override def replacePoints(vertices: Seq[V2]): PointList = PointList(vertices)
  }

}
