package utils.math.planar.algo.straightSkeleton

import org.scalatest.*
import flatspec.*
import matchers.*
import utils.math.planar.algo.straightSkeleton.implhelpers.{Loop, LoopL}
import utils.math.space.V3

class TestShrink extends AnyFlatSpec with should.Matchers {
  "StraightSkeleton" should "work for triangle" in {
    val loop1 = new Loop[Edge]()
    val c1 = new Corner(0, 0)
    val c2 = new Corner(100, 0)
    val c3 = new Corner(100, 100)

    val directionMachine = new Machine()

    loop1.append(new Edge(c1, c2))
    loop1.append(new Edge(c2, c3))
    loop1.append(new Edge(c3, c1))

    for (e <- loop1.iterator)
      e.machine = directionMachine

    val a = new LoopL[Edge](loop1)
    val output = OffsetSkeleton.shrink(a, 5)

    val res: Seq[V3] = output.iterator.toSeq.flatMap(_.iterator.toSeq.map(_.asInstanceOf[Corner].asV3))

    val expected = Seq(
      V3(12.07106781186, 5.000000, 100.000000),
      V3(95.000000, 5.000000, 100.000000),
      V3(95.000000, 87.92893218813, 100.000000),
    )

    assert(res(0) ~= expected(0))
    assert(res(1) ~= expected(1))
    assert(res(2) ~= expected(2))


    for {
      ll <- output.iterator;
      _ = print(">>")
      c <- ll.iterator
    } print(c)
  }
}
