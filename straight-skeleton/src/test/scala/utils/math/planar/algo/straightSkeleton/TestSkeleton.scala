package utils.math.planar.algo.straightSkeleton


import org.scalatest.*
import flatspec.*
import matchers.*
import utils.math.Scalar
import utils.math.planar.algo.straightSkeleton.helpers.SSOps
import utils.math.planar.algo.straightSkeleton.implhelpers.{Loop, LoopL}
import utils.math.space.V3


class TestSkeleton extends AnyFlatSpec with should.Matchers {


  "StraightSkeleton" should "work for triangle with different speeds" in {
    val c1 = new Corner(0, 0)
    val c2 = new Corner(100, -100)
    val c3 = new Corner(100, 0)

    val speed1 = new Machine(Math.PI / 4)
    val speed2 = new Machine(Math.PI / 3)

    val loop1 = new Loop[Edge]


    val e1 = new Edge(c1, c2)
    val e2 = new Edge(c2, c3)
    val e3 = new Edge(c3, c1)

    loop1.append(e1)
    loop1.append(e2)
    loop1.append(e3)

    e1.machine = speed1
    e2.machine = speed1
    e3.machine = speed2

    val skel = new Skeleton(loop1.singleton, true)
    skel.skeleton()

    for (face <- skel.output.faces.values) {
      System.out.println("face:")
      for {lp3 <- face.points.iterator;
           pt <- lp3.iterator} {
        System.out.println(pt)
      }
    }

    val dumped = SSOps.dumpFaces(skel)

    val expected = Seq(
      Seq(
        V3(75.88190451025207, -41.77376677004119, 24.11809548974793),
        V3(0.0, 0.0, 0.0),
        V3(100.0, -100.0, 0.0)
      ),
      Seq(
        V3(75.88190451025207, -41.77376677004119, 24.11809548974793),
        V3(100.0, -100.0, 0.0),
        V3(100.0, 0.0, 0.0)
      ),
      Seq(
        V3(75.88190451025207, -41.77376677004119, 24.11809548974793),
        V3(100.0, 0.0, 0.0),
        V3(0.0, 0.0, 0.0)
      )
    )

    //without order
    assert(SSOps.almostEqWithShifts(dumped, expected))
    //with order
    assert(expected.zip(dumped).forall((e, d) => e.zip(d).forall(_ ~= _)))

  }

  test("work for square")(
    (100, 100),
    (-100, 100),
    (-100, -100),
    (100, -100)
  )(
    Seq(
      V3(-0.0, 0.0, 100.00000000000001),
      V3(100.0, 100.0, 0.0),
      V3(-100.0, 100.0, 0.0)),
    Seq(
      V3(-0.0, 0.0, 100.00000000000001),
      V3(-100.0, 100.0, 0.0),
      V3(-100.0, -100.0, 0.0)),
    Seq(
      V3(-0.0, 0.0, 100.00000000000001),
      V3(-100.0, -100.0, 0.0),
      V3(100.0, -100.0, 0.0)),
    Seq(
      V3(-0.0, 0.0, 100.00000000000001),
      V3(100.0, -100.0, 0.0),
      V3(100.0, 100.0, 0.0)),
  )

  test("work for rectangle")(
    (200, 100),
    (-200, 100),
    (-200, -100),
    (200, -100)
  )(
    Seq(
      V3(-100.0, 0.0, 100.00000000000001),
      V3(100.0, -0.0, 100.00000000000001),
      V3(200.0, 100.0, 0.0),
      V3(-200.0, 100.0, 0.0),
    ),
    Seq(
      V3(-100.0, 0.0, 100.00000000000001),
      V3(-200.0, 100.0, 0.0),
      V3(-200.0, -100.0, 0.0),
    ),
    Seq(
      V3(100.0, -0.0, 100.00000000000001),
      V3(-100.0, 0.0, 100.00000000000001),
      V3(-200.0, -100.0, 0.0),
      V3(200.0, -100.0, 0.0),
    ),
    Seq(
      V3(100.0, -0.0, 100.00000000000001),
      V3(200.0, -100.0, 0.0),
      V3(200.0, 100.0, 0.0),
    ),
  )

  def test(name: String)(vs: (Scalar, Scalar)*)(expected: Seq[V3]*): Unit =
    "StraightSkeleton" should s"$name" in {
      val s = SSOps.setupFor(vs: _*)
      val dumped = SSOps.dumpFaces(s)

      //without order
      assert(SSOps.almostEqWithShifts(dumped, expected))
      //with order
      assert(expected.zip(dumped).forall((e, d) => e.zip(d).forall(_ ~= _)))
    }

}

