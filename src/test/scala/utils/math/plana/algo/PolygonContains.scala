package utils.math.plana.algo
import org.scalatest.funsuite.AnyFunSuite
import utils.datastructures.spatial.AARectangle
import utils.math.planar.V2
import utils.math.planar.algo.PolygonContains

class PolygonContains extends AnyFunSuite {
  test("Simple Contains"){
    System.setProperty("java.util.logging.SimpleFormatter.format", "[%1$tT][%4$-7s] %5$s %n")
    val parent = AARectangle(-10 * V2(1, 1), 10 * V2(1, 1)).toPolygon.asPolygon.asSeq
    val child = AARectangle(-5 * V2(1, 1), 5 * V2(1, 1)).toPolygon.asPolygon.asSeq
    assert(PolygonContains.contains(parent, child))
    assert(!PolygonContains.contains(child, parent))
    assert(PolygonContains.contains(parent, parent))
    assert(PolygonContains.contains(child, child))
  }



}
