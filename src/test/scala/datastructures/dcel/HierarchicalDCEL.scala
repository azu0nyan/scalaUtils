package datastructures.dcel

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.AppendedClues._
import utils.datastructures.dcel.HierarchicalDCEL.HierarchicalFace
import utils.datastructures.spatial.AARectangle
import utils.math.planar.V2

import java.util.concurrent.atomic.AtomicInteger

class HierarchicalDCEL extends AnyFunSuite {
  test(""){
    var x = new AtomicInteger()
    val root = new HierarchicalFace[V2, String, String](None, "ROOT")(x => x)
    val toCut = AARectangle(V2(-100, -100), V2(100, 100)).toPolygon
    root.cutClamped(toCut,
      x => x,
      (a, b) => (x.getAndIncrement().toString, x.getAndIncrement().toString),
      (a, b) => (x.getAndIncrement().toString, x.getAndIncrement().toString),
      a =>  x.getAndIncrement().toString)
    assert(root.allChildFaces.size == 1)
  }
}
