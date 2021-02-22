package old

import org.scalatest.AppendedClues._
import org.scalatest.AppendedClues._
import org.scalatest.funsuite
import org.scalatest.funsuite.AnyFunSuite
import utils.math._
import utils.math.space.intersections.Intersects
import utils.math.space._

import scala.util.Random

class CollisionTests extends AnyFunSuite {
  test("sphere to box simple intersections") {
    {
      val s = Sphere(0f, 1f)
      val b = new AABox(1f)
      assert(Intersects.aaboxToSphere(b, s))
    }
    {
      val s = Sphere(0f, 0.1f)
      val b = new AABox(1f)
      assert(Intersects.aaboxToSphere(b, s))
    }
    {
      val s = Sphere(0f, 1f)
      val b = new AABox(0.1f)
      assert(Intersects.aaboxToSphere(b, s))
    }
  }

  def testSTBWithOffset(offsetSphere: V3 = 0f, offsetBox: V3 = 0f): Boolean = {
    val s = Sphere(offsetSphere, 0.5f)
    val b = AABox(offsetBox - 0.5f, offsetBox + 0.5f)
    Intersects.aaboxToSphere(b, s)
  }

  test("sphere to box sides") {
    V3.positiveNegativeAxises.foreach { a =>
      assert(testSTBWithOffset(a.normalize * 0.9f, 0f))
      assert(testSTBWithOffset(0f, a.normalize * 0.9f))
      assert(!testSTBWithOffset(a.normalize * 1.1f, 0f))
      assert(!testSTBWithOffset(0f, a.normalize * 1.1f))
    }
  }

  test("sphere to box bug") {
    assert(!testSTBWithOffset(0f, V3(-0f, -1f, -0f).normalize * 1.1f))
  }

  test("Sphere to box angles") {
    V3.diagonals.foreach { d =>
      assert(testSTBWithOffset(d.normalize * 1.35f, 0f))
      assert(testSTBWithOffset(0f, d.normalize * 1.35f))
      assert(!testSTBWithOffset(d.normalize * 1.5f, 0f))
      assert(!testSTBWithOffset(0f, d.normalize * 1.5f))
    }
  }
  /////////////////
  test("AABB to self") {
    val box1 = AABox(0f, 10f)
    val box2 = AABox(2f, 9f)
    val box3 = AABox(10f, 20f)
    assert(box1.intersectsWith(box1))
    assert(box1.intersectsWith(box2))
    assert(box1.contains(box2))
    assert(!box1.intersectsWith(box3))
  }

  test("AABB to OBB random converted") {
    val r = new Random(1)
      for (_ <- 0 to 1000) {
        val min1 = randV3InRange(0f, 10f, r)
        val max1 = min1 + randV3InRange(3f, 7f, r)
        val min2 = randV3InRange(0f, 10f, r)
        val max2 = min2 + randV3InRange(3f, 7f, r)
        val box1 = AABox(min1, max1)
        val box2 = AABox(min2, max2)
        val res = box1.intersects(box2)
        val inersection = box1.intersection(box2).volume
        if(Math.abs(inersection) > 0.05f ) {
          assert(Intersects.aaBoxToOBox(box1, box2.orientedBox) == res) withClue (s"intersects:$res intersection:$inersection aa $box1 ob $box2")
          assert(Intersects.aaBoxToOBox(box2, box1.orientedBox) == res) withClue (s"intersects:$res intersection:$inersection aa $box2 ob $box1")
          assert(Intersects.oboxToOBox(box1.orientedBox, box2.orientedBox) == res) withClue (s"intersects:$res intersection:$inersection ob $box1 ob $box2")
          assert(Intersects.oboxToOBox(box2.orientedBox, box1.orientedBox) == res) withClue (s"intersects:$res intersection:$inersection bo $box2 ob $box1")
        }
    }
  }

  test("AABB to OBB rotated") {
    val aaBB = AABox(-.5f, .5f)
    val toRotate = aaBB.transform(Transform(translation = V3(0.95, 0 ,0))).orientedBox
    val toRotate2 = aaBB.transform(Transform(translation = V3(1.7, 0 ,0))).orientedBox
    val count = 50
    for(
      i<- 0 to count;
      j<- 0 to count;
      k<- 0 to count
        ){
      val x = TWO_PI * i / count
      val y = TWO_PI * j / count
      val z = TWO_PI * k / count

      assert(Intersects.aaBoxToOBox(aaBB, toRotate.transform(Transform(rotation = Quat.fromAngles(x, y, z)))))
      assert(Intersects.oboxToOBox(aaBB.orientedBox, toRotate.transform(Transform(rotation = Quat.fromAngles(x, y, z)))))
      assert(!Intersects.aaBoxToOBox(aaBB, toRotate2.transform(Transform(rotation = Quat.fromAngles(x, y, z)))))
      assert(!Intersects.oboxToOBox(aaBB.orientedBox, toRotate2.transform(Transform(rotation = Quat.fromAngles(x, y, z)))))
      assert(!Intersects.oboxToOBox(toRotate2.transform(Transform(rotation = Quat.fromAngles(x, y, z))), aaBB.orientedBox))
    }

  }
  test("OBB to OBB rotated") {
    val aaBB = AABox(-.5f, .5f)
    val toRotateInter = aaBB.transform(Transform(translation = V3(0.99, 0 ,0))).orientedBox
    val toRotateNointer = aaBB.transform(Transform(translation = V3(sqrt(2.01), 0 ,0))).orientedBox
    val toRotate = aaBB.orientedBox
    val count = 10
    for(
      i<- 0 to count;
      j<- 0 to count;
      k<- 0 to count;
      i1<- 0 to count;
      j1<- 0 to count;
      k1<- 0 to count
    ){
      val x = TWO_PI * i / count
      val y = TWO_PI * j / count
      val z = TWO_PI * k / count
      val x1 = TWO_PI * i1 / count
      val y1 = TWO_PI * j1 / count
      val z1 = TWO_PI * k1 / count

      assert(Intersects.oboxToOBox(
        toRotate.transform(Transform(rotation = Quat.fromAngles(x, y, z))),
        toRotateInter.transform(Transform(rotation = Quat.fromAngles(x1, y1, z1))))
      )
      assert(!Intersects.oboxToOBox(
        toRotate.transform(Transform(rotation = Quat.fromAngles(x, y, z))),
        toRotateNointer.transform(Transform(rotation = Quat.fromAngles(x1, y1, z1))))
      )
    }
  }

  test("obox to triangle bug") {
    val obox = OBox(V3(12.5, 12.5, 12.5),(V3(1.0, 0.0, 0.0),V3(0.0, 1.0, 0.0),V3(0.0, 0.0, 1.0)),V3(0.5, 0.5, 0.5))
    val tr = Triangle(V3(12.730878, 12.410081, 12.207715),V3(12.396312, 13.3455925, 11.21995),V3(13.658287, 13.289812, 13.102105))
    assert(Intersects.oboxToTriange(obox, tr))
  }

  test("obox to obox bug") {
    val obox1 = OBox(V3(156.37325, 133.40828, 117.74907),(V3(1.0, 0.0, 0.0),V3(0.0, 1.0, 0.0),V3(0.0, 0.0, 1.0)),V3(0.5, 0.5, 0.5))
    val obox2 = OBox(V3(157.54674, 133.16614, 117.78633),(V3(-0.45149326, 0.8800341, 0.14728773),V3(0.40387177, 0.054364443, 0.91319877),V3(0.79563886, 0.4717886, -0.37996602)),V3(0.5, 0.5, 0.5))
    assert(Intersects.oboxToOBox(obox1, obox2))
    assert(Intersects.oboxToOBox(obox2, obox1))

    val obox3 =OBox(V3(157.54674, 133.16614, 117.78633),(V3(-0.45149326, 0.8800341, 0.14728773),V3(0.40387177, 0.054364443, 0.91319877),V3(0.79563886, 0.4717886, -0.37996602)),V3(0.5, 0.5, 0.5))
    val obox4 = OBox(V3(158.02107, 133.06859, 116.74261),(V3(1.0, 0.0, 0.0),V3(0.0, 1.0, 0.0),V3(0.0, 0.0, 1.0)),V3(0.5, 0.5, 0.5))
    assert(Intersects.oboxToOBox(obox3, obox4))
    assert(Intersects.oboxToOBox(obox4, obox3))

    val obox5 =  OBox(V3(157.54674, 133.16614, 117.78633),(V3(-0.45149326, 0.8800341, 0.14728773),V3(0.40387177, 0.054364443, 0.91319877),V3(0.79563886, 0.4717886, -0.37996602)),V3(0.5, 0.5, 0.5))
    val obox6 = OBox(V3(158.58951, 133.05801, 118.05994),(V3(1.0, 0.0, 0.0),V3(0.0, 1.0, 0.0),V3(0.0, 0.0, 1.0)),V3(0.5, 0.5, 0.5))
    assert(Intersects.oboxToOBox(obox5, obox6))
    assert(Intersects.oboxToOBox(obox6, obox5))
  }
}
