package old

import java.io.{File, PrintWriter}

import org.scalatest.FunSuite
import org.scalatest.AppendedClues._
import org.scalatest.funsuite.AnyFunSuite
import utils.datastructures.IntV2
import utils.math.planar.SegmentsIntersection.{SegmentsOrdering, Status, StatusAVlImpl}
import utils.math.planar.{PointIntersection, SegmentIntersection, SegmentPlanar, SegmentToSegmentPlanarIntersection, SegmentsIntersection, V2}
import utils.math._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object SegmentsIntersectionsTest {

  def randomSegments(count: Int, delta: V2, minLength: Scalar, maxLength: Scalar, seed: Int = 1): Seq[SegmentPlanar] = {
    val r = if (seed == 1) new Random() else new Random(seed)
    for (i <- 0 to count) yield {
      val length = randInRange(minLength, maxLength, r)
      val start = V2(r.nextDouble() * delta.x, r.nextDouble() * delta.y)
      val end = start + V2.ox.rotate(randInRange(0, TWO_PI)) * length
      SegmentPlanar(start, end)
    }
  }

  def grid(gridSize: Int, left: V2 = V2.ox, up: V2 = V2.oy): Seq[SegmentPlanar] = (
    for (i <- 0 to gridSize) yield SegmentPlanar(i.toDouble * up, gridSize.toDouble * left + i.toDouble * up)
    ) ++
    (
      for (i <- 0 to gridSize) yield SegmentPlanar(i.toDouble * left, i.toDouble * left + gridSize.toDouble * up)
      )

  def gridRect(gridSize: IntV2, left: V2 = V2.ox, up: V2 = V2.oy): Seq[SegmentPlanar] = (
    for (i <- 0 to gridSize.x) yield SegmentPlanar(i.toDouble * up, gridSize.y.toDouble * left + i.toDouble * up)
    ) ++
    (
      for (i <- 0 to gridSize.y) yield SegmentPlanar(i.toDouble * left, i.toDouble * left + gridSize.x.toDouble * up)
      )

  def naiveWay(s: Seq[SegmentPlanar]): Set[SegmentToSegmentPlanarIntersection] = if (s.size > 1)
    s.flatMap(seg => s.filter(s2 => s2 != seg).map(s2 => s2.intersection(seg).toSeq).reduce(_ ++ _)).toSet
  else
    Set()

  def test_(testName: String, toTest: Seq[SegmentPlanar]): Boolean =
    testResult(testName, toTest, SegmentsIntersection.findIntersection(toTest).map(i => i._1).toSet,
      naiveWay(toTest).flatMap(i => i match {
        case PointIntersection(p) => Some(p)
        case SegmentIntersection(s) => None
      }).toSet)

  def testResult(testName: String, data: Seq[SegmentPlanar], found: Set[V2], expected: Set[V2]): Boolean = {

    println("---------------------------------------------------------------------------------------")
    println(s"$testName")
    println("DATA")
    println(data)
    val foundToExpected = found.map(f => expected.exists(_ ~= f)).reduceOption(_ && _).getOrElse(found.isEmpty && expected.isEmpty)
    val expectedToFound = expected.map(e => found.exists(_ ~= e)).reduceOption(_ && _).getOrElse(found.isEmpty && expected.isEmpty)

    if (foundToExpected && expectedToFound) {
      println("STATUS:OK")
      println("FOUND & EXPECTED")
      println(found)
      true
    } else {
      println("STATUS:ERROR !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
      println("FOUND")
      println(found)
      println("EXPECTED")
      println(expected)
      println("FOUND - EXPECTED")
      println(found.diff(expected))
      println("EXPECTED - FOUND")
      println(expected.diff(found))
      println("FOUND && EXPECTED")
      println(expected.intersect(found))
      false
    }

  }
}

class SegmentsIntersectionsTest extends AnyFunSuite {

  val l = V2(-1, 0)
  val r = V2(1, 0)
  val u = V2(0, 1)
  val d = V2(0, -1)

  val lr = SegmentPlanar(l, r)
  val ud = SegmentPlanar(u, d)


  test("segmentsOrdering") {
    val o = new SegmentsOrdering
    o.sweepPoint = V2(0f, 0f)

    assert(o.gt(lr, ud))
    assert(o.lt(ud, lr))
    assert(o.lt(lr, lr + V2(1, 0)))
    assert(o.gt(lr, lr - V2(1f, 0)))
    assert(o.lt(lr - V2(5, 0), lr))
    assert(o.gt(lr + V2(5, 0), lr))
    assert(o.lt(SegmentPlanar(V2(-1, 1), V2(1, -1)), SegmentPlanar(V2(1, 1), V2(-1, -1))))
    assert(o.gt(SegmentPlanar(V2(1, 1), V2(-1, -1)), SegmentPlanar(V2(-1, 1), V2(1, -1))))
    assert(o.lt(SegmentPlanar(V2(-1, 1), V2(1, -1)) - V2(0.1f, 0), SegmentPlanar(V2(1, 1), V2(-1, -1))))
    assert(o.gt(SegmentPlanar(V2(1, 1), V2(-1, -1)), SegmentPlanar(V2(-1, 1), V2(1, -1)) - V2(0.1f, 0)))
  }

  test("basic status ops") {
    val s = new StatusAVlImpl
    val count = 100
    val segments = for (i <- 0 until count) yield SegmentPlanar(i, -1, i + 10, 1)

    s.add(segments(0), segments(0).v1)
    assert(s.storage.values.toSet.nonEmpty)
    s.removeSegmentsContainsPoint(segments(0).v1)
    assert(s.storage.values.toSet.isEmpty)

    s.add(segments(0), segments(0).v1)
    assert(s.storage.values.toSet.nonEmpty)
    s.removeSegmentsContainsPoint(segments(0).v2)
    assert(s.storage.values.toSet.isEmpty)

    s.add(segments(0), segments(0).v1)
    assert(s.storage.values.toSet.nonEmpty)
    s.removeSegmentsContainsPoint(segments(0).center)
    assert(s.storage.values.toSet.isEmpty)


  }

  test("simple status tests") {
    val count = 100
    val s = new StatusAVlImpl
    val sp = V2(0, 0)
    val control: mutable.HashSet[SegmentPlanar] = new mutable.HashSet[SegmentPlanar]()
    val segments = for (i <- 0 until count) yield SegmentPlanar(i, -1, i, 1)
    s.add(segments, sp)
    control ++= segments
    for (i <- 0 until count) {
      s.removeSegmentsContainsPoint(segments(i).v1)
      control -= segments(i)
      assert(s.storage.size == count - i - 1) withClue s" $i"
      assert(s.storage.valuesOrdered.toSet == control.toSet)
    }
    s.add(segments, sp)
    for (i <- 0 until count) {
      s.removeSegmentsContainsPoint(segments(i).v2)
      assert(s.storage.size == count - i - 1) withClue s" $i"
    }
    s.add(segments, sp)
    for (i <- 0 until count) {
      s.removeSegmentsContainsPoint(segments(i).center)
      assert(s.storage.size == count - i - 1) withClue s" $i"
    }

    s.add(segments, sp)
    var removed = 0
    val is: ArrayBuffer[Int] = new ArrayBuffer[Int]
    is ++= (0 until count)
    Random.shuffle(is)


    for (i <- is) {
      s.removeSegmentsContainsPoint(segments(i).center)
      removed += 1
      assert(s.storage.size == count - removed)
    }

  }
  test("randomized tes") {
    val filename = "wrongBo"
    val segsMin = 2
    val segsMax = 9

    for (i <- 0 to 1000) {
      val count = randIntInRangeInclusive(segsMin, segsMax)
      val segs = SegmentsIntersectionsTest.randomSegments(count, V2(20, 20), 10f, 20f, i)
      if (!SegmentsIntersectionsTest.test_(s"$i", segs)) {
        println(s"storing to : $filename")
        val f = new File(filename)
        val pw = new PrintWriter(f)

        segs.foreach(s => pw.println(s"${s.v1.x} ${s.v1.y} ${s.v2.x} ${s.v2.y}"))
        pw.close()
        assert(false)
      }
    }
  }

  test("SIMPLE INTERSECTIONS TESTS") {
    assert(SegmentsIntersectionsTest.test_("SIMPLE INTERSECTIONS TESTS", Seq(lr, ud)))
  }


  test("SKEW GRID TEST 1x1") {
    val gr = SegmentsIntersectionsTest.grid(1, left = V2(1, 1), up = (V2(1, -1)))
    // DebugDrawing.startDrawing()
    // gr.foreach(l => DebugDrawing.addObject(new DrawableSegment(l)))
    assert(SegmentsIntersectionsTest.test_("SKEW GRID TEST 1x1", gr))
  }

  test("GRID TEST 1x1") {
    assert(SegmentsIntersectionsTest.test_("GRID TEST 1x1", SegmentsIntersectionsTest.grid(1)))
  }

  test("GRID TEST 2x2") {
    assert(SegmentsIntersectionsTest.test_("GRID TEST 2x2", SegmentsIntersectionsTest.grid(2)))
  }
  test("GRID TEST 5x5") {
    assert(SegmentsIntersectionsTest.test_("GRID TEST 5x5", SegmentsIntersectionsTest.grid(5)))
  }


  test("segment bug") {
    println(SegmentPlanar(V2(-100, 100), V2(-100, 0)).intersection(SegmentPlanar(V2(-100, 200), V2(-100, 400))))
    assert(SegmentPlanar(V2(-100, 100), V2(-100, 0)).intersection(SegmentPlanar(V2(-100, 200), V2(-100, 400))).isEmpty)
  }

}
