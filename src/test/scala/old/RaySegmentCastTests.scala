package old

import org.scalatest.FunSuite
import org.scalatest.AppendedClues._
import org.scalatest.funsuite.AnyFunSuite
import utils.datastructures.spatial.GridRasterization
import utils.math._
import utils.math.planar.V2
import utils.math.space.intersections.{LineRayToTrinagleIntersection, RaySegmentCast}
import utils.math.space.intersections.RaySegmentCast._
import utils.math.space._

class RaySegmentCastTests extends AnyFunSuite {
  test("RayTriangleCast") {
    val triangle1 = Triangle(V2.ox.planarToV3(0), V2.ox.rotate(2f * Math.PI.toFloat / 3f).planarToV3(0), V2.ox.rotate(4f * Math.PI.toFloat / 3f).planarToV3(0))

    val oyRay = Ray(V3(0, 10, 0), V3(0, -1, 0))
    val oyRay2 = Ray(V3(10, 10, 10), V3(10, -1, 10))
    assert(LineRayToTrinagleIntersection.rayToTriangle(oyRay, triangle1).nonEmpty)
    assert(LineRayToTrinagleIntersection.rayToTriangle(oyRay, triangle1).get ~= V3.ZERO)
    assert(LineRayToTrinagleIntersection.rayToTriangle(oyRay2, triangle1).isEmpty)
  }

  def testSegmentToAA(box: AABox, seg: Segment, expected: Option[V3]) = {
    val cast: CastResult = SegmentCast.toAABox(seg, box)
    assert(cast.nonEmpty == expected.nonEmpty)
    cast match {
      case Some(res) => assert(RaySegmentCast.location(res) ~= expected.get) withClue s"$box exp $expected $seg result $cast"
      case None =>
    }
  }

  test("RayAABoxTest") {
    val centeredBox = new AABox(2f) //from -1 to 1
    val oxSegment = Segment(-V3.x * 10, V3.x * 10)
    val oySegment = Segment(-V3.y * 10, V3.y * 10)
    val ozSegment = Segment(-V3.z * 10, V3.z * 10)
    for (
      i <- BigDecimal(-0.99f) to BigDecimal(0.99f) by (0.01d) map (_.toFloat);
      j <- BigDecimal(-0.99f) to BigDecimal(0.99f) by (0.01d) map (_.toFloat)
    ) {
      testSegmentToAA(centeredBox, oxSegment.transform(Transform().move(V3(0, i, j))), Some(V3(-1, i, j)))
      testSegmentToAA(centeredBox, oxSegment.transform(Transform().move(V3(0, i, j))).flip, Some(V3(1, i, j)))
      testSegmentToAA(centeredBox, oySegment.transform(Transform().move(V3(i, 0, j))), Some(V3(i, -1, j)))
      testSegmentToAA(centeredBox, oySegment.transform(Transform().move(V3(i, 0, j))).flip, Some(V3(i, 1, j)))
      testSegmentToAA(centeredBox, ozSegment.transform(Transform().move(V3(i, j, 0))), Some(V3(i, j, -1)))
      testSegmentToAA(centeredBox, ozSegment.transform(Transform().move(V3(i, j, 0))).flip, Some(V3(i, j, 1)))
    }
    for (
      i <- (BigDecimal(-2f) to BigDecimal(-1.01f) by (0.01d) map (_.toFloat))
        ++
        (BigDecimal(1.01f) to BigDecimal(2.0f) by (0.01d) map (_.toFloat));
      j <- (BigDecimal(-2f) to BigDecimal(-1.01f) by (0.01d) map (_.toFloat))
        ++
        (BigDecimal(1.01f) to BigDecimal(2.0f) by (0.01d) map (_.toFloat))
    ) {
      testSegmentToAA(centeredBox, oxSegment.transform(Transform().move(V3(0, i, j))), None)
      testSegmentToAA(centeredBox, oxSegment.transform(Transform().move(V3(0, i, j))).flip, None)
      testSegmentToAA(centeredBox, oySegment.transform(Transform().move(V3(i, 0, j))), None)
      testSegmentToAA(centeredBox, oySegment.transform(Transform().move(V3(i, 0, j))).flip, None)
      testSegmentToAA(centeredBox, ozSegment.transform(Transform().move(V3(i, j, 0))), None)
      testSegmentToAA(centeredBox, ozSegment.transform(Transform().move(V3(i, j, 0))).flip, None)
    }
    //NumericRange(0f, 1f, 45f)
    //val v4 = 1.1f to 10.3f by 3.1f

    //for(i <- ((-0.99f) to 0.99f by 0.01f))
    //for(i <- Range.Float(-0.99f, 0.99f, 0.01f))

  }

  test("bug? segmentTest") {
    val s = Segment(V3(63.86415f, 63.734837f, 54.374878f), V3(-693.2076f, -686.7418f, -218.74416f))
    val box = AABox(V3(57.16317f, 61.485893f, 51.691788f), V3(58.16317f, 62.485893f, 52.691788f))
    val wrongPoint = V3(58.16317f, 58.08352f, 52.318207f)
    val result = SegmentCast.toAABox(s, box)
    assert(result.isEmpty || (box.distanceTo(result.get._1) ~= 0f))
  }

  test("bug? segmentTest2") {
    val s = Segment(V3(63, 63, 54), V3(-69, -68, -21))
    val box = AABox(V3(57, 61, 51), V3(58, 62, 52))
    //val wrongPoint = V3(58.16317f, 58.08352f, 52.318207f)
    val result = SegmentCast.toAABox(s, box)
    assert(result.isEmpty || (box.distanceTo(result.get._1) ~= 0f))
  }

  test("no hit bug") {
    val box = AABox(V3(17.237936f, 17.188265f, 20.44083f), V3(18.237936f, 18.188265f, 21.44083f))
    val segments = Seq(
      Segment(V3(21.486113, 17.017515, 27.36899), V3(16.887732, 17.827194, 18.525953)),
      Segment(V3(16.26003, 17.643139, 19.005316), V3(23.82817, 16.82373, 25.490053)),
      Segment(V3(19.975277, 22.044249, 16.202654), V3(16.61204, 15.857979, 23.303267)),
      Segment(V3(17.801329, 18.822096, 19.944866), V3(18.31821, 12.048607, 27.283306)),
      Segment(V3(17.801329, 18.822096, 19.944866), V3(19.298363, 12.651106, 27.670006)),
      Segment(V3(17.801329, 18.822096, 19.944866), V3(18.526674, 13.630141, 28.460587)),
      Segment(V3(17.801329, 18.822096, 19.944866), V3(14.926456, 14.514598, 28.499432)),
      Segment(V3(17.801329, 18.822096, 19.944866), V3(13.863066, 12.378346, 26.499878)),
      Segment(V3(17.801329, 18.822096, 19.944866), V3(15.652971, 10.641552, 25.279963)),
      Segment(V3(17.801329, 18.822096, 19.944866), V3(19.000721, 10.086277, 24.66153)),
      Segment(V3(17.801329, 18.822096, 19.944866), V3(19.07717, 9.442106, 23.167912)),
      Segment(V3(17.801329, 18.822096, 19.944866), V3(16.061543, 9.688814, 23.626757))
    )

    segments.foreach { s =>
      val result = SegmentCast.toAABox(s, box)
      println(result.toString + " " + s)
      assert(result.nonEmpty && (box.distanceTo(result.get._1) ~= 0f)) withClue s"box $box segment $s result $result"
    }
  }

  test("segment rasterization") {
    val seg = Segment(V3(0, 0, 0), V3(25, 0, 0))
    val raster = GridRasterization.rasterize3d(seg, 10).toSeq
    println(raster)
    assert(raster == Seq((0, 0, 0), (1, 0, 0), (2, 0, 0)))
  }

  test("segment rasterization2") {
    val seg = Segment(V3(0, 8, 0), V3(0, 15, 7))
    val raster = GridRasterization.rasterize3d(seg, 10).toSeq
    println(raster)
    assert(raster == Seq((0, 0, 0), (0, 1, 0)))
  }

  test("segment rasterization3") {
    val seg = Segment(V3(18.246313, 3.312324, -21.764006),V3(565.9536, 640.2281, 520.78107))
    val raster = GridRasterization.rasterize3d(seg, 10).toSeq
    println(raster)
    assert(!raster.flatMap(v => Seq(v._1, v._2, v._3)).exists(x => x < -30))
  }

  test("aaboxNormalAt") {
    val seg = Segment(V3(21.798136f, 11.344841f, 31.80302f), V3(884.86163f, 172.74496f, 510.4172f))
    val box = AABox(V3(25.895462f, 11.836564f, 34.28104f), V3(26.895462f, 12.836564f, 35.28104f))

    val castResult = SegmentCast.toAABox(seg, box)

    val normalAt = normal(castResult.get)
    assert(normalAt == V3(0, 0, -1))
  }

  test("to triangle"){
    val seg = Segment(V3(-10.0f, 0.0f, 0.0f),V3(10f, 0.0f, 0.0f))
    val tr = Triangle(V3(0.0f, -1f, -1f), V3(0f, 1f, 0f), V3(0f, 0f, 1f))

    val castResult = SegmentCast.toTriangle(seg, tr)
    assert(castResult.nonEmpty)
    assert(location(castResult.get) == V3.ZERO)
  }

  test("to triangle bug"){
    val seg = Segment(V3(0.0f, 0.0f, 10.0f),V3(0.9941285f, 0.25387925f, 0.052776337f))
    val tr = Triangle(V3(0.92351276f, -0.35967675f, -1.1691406f),V3(0.5889469f, 0.5758351f, -2.1569061f),V3(1.8509223f, 0.520054f, -0.27475077f))

    val castResult = SegmentCast.toTriangle(seg, tr)
    assert(castResult.nonEmpty)
  }

  test("segment to sphere bug"){
    val sphere = Sphere(V3(0.92351276f, -0.35967675f, -1.1691406f), 0.9990756f)
    val segs = Seq(
      Segment(V3(0.0, 0.0, 10.0),V3(92.639854, 3.9062686, -985.6919)),
      Segment(V3(-9.259086, -1.3272469E-8, 8.245099),V3(722.4303, -30.268753, -672.72076)),
      Segment(V3(-13.926127, -1.0928013E-6, -0.11770216),V3(983.6362, -11.718696, -68.90706))
    )
    for(seg <- segs) {
      val castResult = SegmentCast.toSphere(seg, sphere)
      println(castResult)
      assert(castResult.nonEmpty)
      assert(sphere.distanceTo(location(castResult.get)) ~= 0f)
    }
    for(seg <- segs) {
      val castResult = SegmentCast.toShape(seg, sphere)
      println(castResult)
      assert(castResult.nonEmpty)
      assert(sphere.distanceTo(location(castResult.get)) ~= 0f)
    }
  }

 test("segment to sphere bug2"){
    val sphere =Sphere(V3(0.92351276, -0.35967675, -1.1691406),0.9990756)
    val seg =Segment(V3(0.0, 0.0, 10.0),V3(105.20835, -35.14901, -983.8288))

   val castResult = SegmentCast.toShape(seg, sphere)
   println(castResult)
   assert(castResult.nonEmpty)
   assert(sphere.distanceTo(location(castResult.get)) ~= 0f)
  }

  test("segment to sphere bug3"){
    val sphere =Sphere(V3(34.91184, 31.708736, 38.141975),1.0579108)
    val seg =Segment(V3(-6.1182094, 7.3186407, 27.178947),V3(830.7005, 509.7182, 244.73145))

    val castResult = SegmentCast.toShape(seg, sphere)
    println(castResult)
    assert(castResult.nonEmpty)
    assert(sphere.distanceTo(location(castResult.get)) ~= 0f)
  }

  /*test("segment to aabox bug n"){
   val box = AABox(V3(130.69632, 144.24188, 83.66112),V3(131.69632, 145.24188, 84.66112))

  }*/

}
