package utils.math.plana.algo
import org.scalatest.funsuite.AnyFunSuite
import utils.datastructures.spatial.AARectangle
import utils.math.planar.{Polygon, PolygonRegion, SegmentPlanar, V2}
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


  test("Non axis alligned overlapped quads") {
    System.setProperty("java.util.logging.SimpleFormatter.format", "[%1$tT][%4$-7s] %5$s %n")
    val parent: Polygon = Polygon(List(PolygonRegion(List(V2(-249.0, 260.0), V2(-259.0, 88.0), V2(163.0, 65.0), V2(210.0, 351.0)))))
    val child: Polygon = Polygon(List(PolygonRegion(List(V2(91.0, 405.0), V2(125.0, 247.0), V2(270.0, 256.0), V2(328.0, 443.0)))))
    assert(!PolygonContains.contains(parent.asSeq, child.asSeq))
    assert(!PolygonContains.contains(child.asSeq, parent.asSeq))
    assert(PolygonContains.contains(parent.asSeq, parent.asSeq))
    assert(PolygonContains.contains(child.asSeq, child.asSeq))
  }

  test("Non axis alligned parent contains quads") {
    System.setProperty("java.util.logging.SimpleFormatter.format", "[%1$tT][%4$-7s] %5$s %n")
    val parent: Polygon = Polygon(List(PolygonRegion(List(V2(-265.0, 188.0), V2(-258.0, 69.0), V2(244.0, 77.0), V2(249.0, 333.0)))))
    val child: Polygon = Polygon(List(PolygonRegion(List(V2(-99.0, 207.0), V2(-71.0, 135.0), V2(145.0, 195.0), V2(102.0, 237.0)))))

    assert(PolygonContains.contains(parent.asSeq, child.asSeq))
    assert(!PolygonContains.contains(child.asSeq, parent.asSeq))
    assert(PolygonContains.contains(parent.asSeq, parent.asSeq))
    assert(PolygonContains.contains(child.asSeq, child.asSeq))
  }


  test("NON convex not contains ") {
    System.setProperty("java.util.logging.SimpleFormatter.format", "[%1$tT][%4$-7s] %5$s %n")
    val parent: Polygon = Polygon(List(PolygonRegion(List(V2(-165.0, 153.0), V2(151.0, 149.0), V2(153.0, 301.0), V2(31.0, 348.0), V2(-141.0, 316.0), V2(-246.0, 237.0), V2(-290.0, 164.0), V2(-252.0, 4.0), V2(-185.0, -34.0), V2(-21.0, -22.0), V2(77.0, -12.0), V2(253.0, 4.0), V2(209.0, 73.0), V2(56.0, 34.0), V2(-42.0, 4.0), V2(-139.0, 4.0), V2(-188.0, 17.0), V2(-213.0, 62.0), V2(-223.0, 89.0), V2(-230.0, 123.0), V2(-232.0, 162.0), V2(-207.0, 215.0), V2(-159.0, 264.0), V2(-89.0, 289.0), V2(21.0, 298.0), V2(85.0, 279.0), V2(119.0, 221.0), V2(101.0, 186.0), V2(-39.0, 178.0), V2(-88.0, 190.0), V2(-118.0, 193.0), V2(-148.0, 189.0), V2(-173.0, 176.0)))))
    val child: Polygon = Polygon(List(PolygonRegion(List(V2(-63.0, -92.0), V2(-68.0, -152.0), V2(11.0, -149.0), V2(16.0, -80.0))), PolygonRegion(List(V2(-49.0, 482.0), V2(-41.0, 431.0), V2(9.0, 438.0), V2(36.0, 488.0))), PolygonRegion(List(V2(-485.0, 150.0), V2(-483.0, 108.0), V2(-395.0, 121.0), V2(-408.0, 178.0))), PolygonRegion(List(V2(208.0, 169.0), V2(197.0, 125.0), V2(283.0, 119.0), V2(274.0, 172.0))), PolygonRegion(List(V2(5.0, 115.0), V2(11.0, 62.0), V2(74.0, 79.0), V2(68.0, 127.0))), PolygonRegion(List(V2(-135.0, 127.0), V2(-150.0, 69.0), V2(-79.0, 71.0), V2(-65.0, 115.0))), PolygonRegion(List(V2(-74.0, 255.0), V2(-32.0, 225.0), V2(10.0, 260.0), V2(-37.0, 278.0)))))

    assert(!PolygonContains.contains(parent.asSeq, child.asSeq))
    assert(!PolygonContains.contains(child.asSeq, parent.asSeq))
    assert(PolygonContains.contains(parent.asSeq, parent.asSeq))
    assert(PolygonContains.contains(child.asSeq, child.asSeq))

  }
  //todo fix
  test("Non axis alligned non convex intersects BUG") {
    System.setProperty("java.util.logging.SimpleFormatter.format", "[%1$tT][%4$-7s] %5$s %n")
    val parent: Polygon = Polygon(List(PolygonRegion(List(V2(-445.0, 196.0), V2(-415.0, -6.0), V2(46.0, -117.0), V2(327.0, 200.0), V2(-51.0, 192.0), V2(-99.0, 91.0)))))
    val child: Polygon = Polygon(List(PolygonRegion(List(V2(-394.0, 81.0), V2(-238.0, 3.0), V2(63.0, -15.0), V2(207.0, 188.0)))))


    assert(!PolygonContains.contains(parent.asSeq, child.asSeq))
    assert(!PolygonContains.contains(child.asSeq, parent.asSeq))
    assert(PolygonContains.contains(parent.asSeq, parent.asSeq))
    assert(PolygonContains.contains(child.asSeq, child.asSeq))
  }
  test("Axis aligned concave intersects") {
    System.setProperty("java.util.logging.SimpleFormatter.format", "[%1$tT][%4$-7s] %5$s %n")
    val parent: Polygon = Polygon(List(PolygonRegion(List(V2(-500.0, 300.0), V2(-500.0, -100.0), V2(100.0, -100.0), V2(100.0, 300.0), V2(-100.0, 300.0), V2(-100.0, 100.0), V2(-200.0, 100.0), V2(-200.0, 300.0)))))
    val child: Polygon = Polygon(List(PolygonRegion(List(V2(-300.0, 200.0), V2(-300.0, 0.0), V2(0.0, 0.0), V2(0.0, 200.0)))))

    assert(!PolygonContains.contains(parent.asSeq, child.asSeq))
    assert(!PolygonContains.contains(child.asSeq, parent.asSeq))
    assert(PolygonContains.contains(parent.asSeq, parent.asSeq))
    assert(PolygonContains.contains(child.asSeq, child.asSeq))
  }

  //todo fix
  test("Axis aligned concave intersects 2 BUG") {
    System.setProperty("java.util.logging.SimpleFormatter.format", "[%1$tT][%4$-7s] %5$s %n")
    val parent: Polygon = Polygon(List(PolygonRegion(List(V2(-200.0, -100.0), V2(100.0, -100.0), V2(100.0, 200.0)))))
    val child: Polygon = Polygon(List(PolygonRegion(List(V2(0.0, 200.0), V2(0.0, 0.0), V2(-200.0, 0.0)))))

    assert(!PolygonContains.contains(parent.asSeq, child.asSeq))
    assert(!PolygonContains.contains(child.asSeq, parent.asSeq))
    assert(PolygonContains.contains(parent.asSeq, parent.asSeq))
    assert(PolygonContains.contains(child.asSeq, child.asSeq))
  }
  test("Axis aligned concave intersects 2 BUG case") {
    System.setProperty("java.util.logging.SimpleFormatter.format", "[%1$tT][%4$-7s] %5$s %n")
    val parent: Polygon = Polygon(List(PolygonRegion(List(V2(-200.0, -100.0), V2(100.0, -100.0), V2(100.0, 200.0)))))
    val child: Polygon = Polygon(List(PolygonRegion(List(V2(0.0, 200.0), V2(0.0, 0.0), V2(-200.0, 0.0)))))

    assert(!PolygonContains.contains(child.asSeq, parent.asSeq))
  }
  test("Parent contains child touching borders"){
    System.setProperty("java.util.logging.SimpleFormatter.format", "[%1$tT][%4$-7s] %5$s %n")
    var parent: Polygon = Polygon(List(PolygonRegion(List(V2(0.0, 0.0), V2(300.0, 0.0), V2(300.0, 200.0), V2(0.0, 200.0)))))
    var child: Polygon = Polygon(List(PolygonRegion(List(V2(100.0, 200.0), V2(0.0, 100.0), V2(100.0, 0.0), V2(200.0, 0.0), V2(300.0, 100.0), V2(200.0, 200.0)))))
    assert(PolygonContains.contains(parent.asSeq, child.asSeq))
//    assert(!PolygonContains.contains(child.asSeq, parent.asSeq))
//    assert(PolygonContains.contains(parent.asSeq, parent.asSeq))
//    assert(PolygonContains.contains(child.asSeq, child.asSeq))
  }

  test("Parent contains child touching borders non aligned bug"){
    System.setProperty("java.util.logging.SimpleFormatter.format", "[%1$tT][%4$-7s] %5$s %n")
    val parent: Polygon = Polygon(List(PolygonRegion(List(V2(-200.0, 100.0), V2(0.0, 300.0), V2(200.0, 100.0), V2(0.0, -100.0)))))
    val child: Polygon = Polygon(List(PolygonRegion(List(V2(-58.00000000000003, 241.99999999999997), V2(146.99999999999997, 153.00000000000003), V2(-149.49999999999997, 49.49999999999997)))))
    assert(PolygonContains.contains(parent.asSeq, child.asSeq))//!!!
    assert(!PolygonContains.contains(child.asSeq, parent.asSeq))
    assert(PolygonContains.contains(parent.asSeq, parent.asSeq))
    assert(PolygonContains.contains(child.asSeq, child.asSeq))
  }
  
//  test("YFormXSainity") {
//    assert(SegmentPlanar(V2(-200, 0), V2(0, 200)).yFromX(-200))
//  }

  test("Less At test"){
    val at = -149.49999999999997
    val top = SegmentPlanar(V2(-200.0, 100.0),V2(0.0, 300.0))
    val middle1 = SegmentPlanar(V2(-149.49999999999997, 49.49999999999997),V2(-58.00000000000003, 241.99999999999997))
    val middle2 = SegmentPlanar(V2(-149.49999999999997, 49.49999999999997),V2(146.99999999999997, 153.00000000000003))
    val bot = SegmentPlanar(V2(-200.0, 100.0),V2(0.0, -100.0))




    assert(PolygonContains.lessAt(at, bot, top))
    assert(PolygonContains.lessAt(at, bot, middle1))
    assert(PolygonContains.lessAt(at, bot, middle2))
    assert(!PolygonContains.lessAt(at, bot, bot))

    assert(PolygonContains.lessAt(at, middle2, top))
    assert(PolygonContains.lessAt(at, middle2, middle1))
    assert(!PolygonContains.lessAt(at, middle2, middle2))
    assert(!PolygonContains.lessAt(at, middle2, bot))

    assert(PolygonContains.lessAt(at, middle1, top))
    assert(!PolygonContains.lessAt(at, middle1, middle1))
    assert(!PolygonContains.lessAt(at, middle1, middle2))
    assert(!PolygonContains.lessAt(at, middle1, bot))

    assert(!PolygonContains.lessAt(at, top, top))
    assert(!PolygonContains.lessAt(at, top, middle1))
    assert(!PolygonContains.lessAt(at, top, middle2))
    assert(!PolygonContains.lessAt(at, top, bot))
  }

  test("Parent cross contains center"){
    System.setProperty("java.util.logging.SimpleFormatter.format", "[%1$tT][%4$-7s] %5$s %n")
    val parent: Polygon = Polygon(List(PolygonRegion(List(V2(-100.0, 300.0), V2(0.0, 300.0), V2(0.0, 400.0), V2(100.0, 400.0), V2(100.0, 300.0), V2(200.0, 300.0), V2(200.0, 200.0), V2(100.0, 200.0), V2(100.0, 100.0), V2(0.0, 100.0), V2(0.0, 200.0), V2(-100.0, 200.0)))))
    val child: Polygon = Polygon(List(PolygonRegion(List(V2(0.0, 300.0), V2(0.0, 200.0), V2(100.0, 200.0), V2(100.0, 300.0)))))
    assert(PolygonContains.contains(parent.asSeq, child.asSeq))//!!!
    assert(!PolygonContains.contains(child.asSeq, parent.asSeq))
    assert(PolygonContains.contains(parent.asSeq, parent.asSeq))
    assert(PolygonContains.contains(child.asSeq, child.asSeq))
  }

  test("Parent penta-cross contains center"){
    System.setProperty("java.util.logging.SimpleFormatter.format", "[%1$tT][%4$-7s] %5$s %n")
    val parent: Polygon = Polygon(List(PolygonRegion(List(V2(-100.0, 300.0), V2(-100.0, 400.0), V2(-200.0, 400.0), V2(-200.0, 300.0), V2(-100.0, 300.0), V2(-100.0, 200.0), V2(-200.0, 200.0), V2(-200.0, 100.0), V2(-100.0, 100.0), V2(-100.0, 200.0), V2(0.0, 200.0), V2(0.0, 100.0), V2(100.0, 100.0), V2(100.0, 200.0), V2(0.0, 200.0), V2(0.0, 300.0), V2(100.0, 300.0), V2(100.0, 400.0), V2(0.0, 400.0), V2(0.0, 300.0)))))
    val child: Polygon = Polygon(List(PolygonRegion(List(V2(0.0, 300.0), V2(-100.0, 300.0), V2(-100.0, 200.0), V2(0.0, 200.0)))))
    assert(PolygonContains.contains(parent.asSeq, child.asSeq))//!!!
    assert(!PolygonContains.contains(child.asSeq, parent.asSeq))
    assert(PolygonContains.contains(parent.asSeq, parent.asSeq))
    assert(PolygonContains.contains(child.asSeq, child.asSeq))
  }

  test("Parent contains child in hole"){
    System.setProperty("java.util.logging.SimpleFormatter.format", "[%1$tT][%4$-7s] %5$s %n")
    val parent: Polygon = Polygon(List(PolygonRegion(List(V2(-400.0, 400.0), V2(100.0, 400.0), V2(100.0, 0.0), V2(-400.0, 0.0))), PolygonRegion(List(V2(-500.0, 500.0), V2(200.0, 500.0), V2(200.0, -100.0), V2(-500.0, -100.0)))))
    val child: Polygon = Polygon(List(PolygonRegion(List(V2(-300.0, 300.0), V2(-300.0, 100.0), V2(0.0, 100.0), V2(0.0, 300.0)))))
    assert(!PolygonContains.contains(parent.asSeq, child.asSeq))
    assert(!PolygonContains.contains(child.asSeq, parent.asSeq))
    assert(PolygonContains.contains(parent.asSeq, parent.asSeq))
    assert(PolygonContains.contains(child.asSeq, child.asSeq))
  }

  test("Parent and child shouldnt intersects only touches"){
    System.setProperty("java.util.logging.SimpleFormatter.format", "[%1$tT][%4$-7s] %5$s %n")
    val parent: Polygon = Polygon(List(PolygonRegion(List(V2(-400.0, 300.0), V2(-100.0, 300.0), V2(-100.0, 0.0))), PolygonRegion(List(V2(-400.0, 300.0), V2(-100.0, 0.0)))))
    val child: Polygon = Polygon(List(PolygonRegion(List(V2(-200.0, 100.0), V2(-200.0, 0.0), V2(-100.0, 0.0))), PolygonRegion(List(V2(-400.0, 300.0), V2(-400.0, 100.0), V2(-100.0, -200.0), V2(200.0, 100.0), V2(0.0, 100.0), V2(-100.0, 0.0), V2(-200.0, 0.0), V2(-200.0, 100.0)))))
    assert(!PolygonContains.contains(parent.asSeq, child.asSeq))
    assert(!PolygonContains.contains(child.asSeq, parent.asSeq))
    assert(PolygonContains.contains(parent.asSeq, parent.asSeq))
    assert(PolygonContains.contains(child.asSeq, child.asSeq))
  }

  test("Parent should contain adjanced childs "){
    System.setProperty("java.util.logging.SimpleFormatter.format", "[%1$tT][%4$-7s] %5$s %n")
    var parent: Polygon = Polygon(Seq(PolygonRegion(Seq(V2(-300.0, 200.0), V2(-100.0, 0.0), V2(0.0, 100.0), V2(0.0, -100.0), V2(-100.0, -200.0), V2(-400.0, 100.0), V2(-400.0, 200.0), V2(-400.0, 300.0), V2(0.0, 300.0)))))
    var child: Polygon = Polygon(List(PolygonRegion(List(V2(-300.0, 200.0), V2(-300.0, 0.0), V2(-100.0, -200.0), V2(0.0, -100.0), V2(0.0, 100.0), V2(-100.0, 0.0), V2(-200.0, 0.0), V2(-200.0, 100.0))), PolygonRegion(List(V2(-200.0, 100.0), V2(-200.0, 0.0), V2(-100.0, 0.0)))))

    assert(PolygonContains.contains(parent.asSeq, child.asSeq))
    assert(!PolygonContains.contains(child.asSeq, parent.asSeq))
    assert(PolygonContains.contains(parent.asSeq, parent.asSeq))
    assert(PolygonContains.contains(child.asSeq, child.asSeq))
  }

  test("Parent should contain adjanced childs 2"){
    System.setProperty("java.util.logging.SimpleFormatter.format", "[%1$tT][%4$-7s] %5$s %n")
    var parent: Polygon = Polygon(List(PolygonRegion(List(V2(-500.0, 200.0), V2(-100.0, -200.0), V2(300.0, 200.0), V2(-100.0, 600.0), V2(-300.0, 400.0), V2(0.0, 100.0), V2(-100.0, 0.0), V2(-400.0, 300.0)))))
    var child: Polygon = Polygon(List(PolygonRegion(List(V2(-300.0, 200.0), V2(-300.0, 0.0), V2(-100.0, -200.0), V2(0.0, -100.0), V2(0.0, 100.0), V2(-100.0, 0.0), V2(-200.0, 0.0), V2(-200.0, 100.0))), PolygonRegion(List(V2(-200.0, 100.0), V2(-200.0, 0.0), V2(-100.0, 0.0)))))

    assert(PolygonContains.contains(parent.asSeq, child.asSeq))

    assert(!PolygonContains.contains(child.asSeq, parent.asSeq))
    assert(PolygonContains.contains(parent.asSeq, parent.asSeq))
    assert(PolygonContains.contains(child.asSeq, child.asSeq))
  }



}
