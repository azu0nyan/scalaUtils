package utils.math.planar.algo.straightSkeleton


import org.scalatest.*
import flatspec.*
import matchers.*
import utils.datastructures.CircullarOps.asCyclicPairs
import utils.math.Scalar
import utils.math.planar.V2
import utils.math.planar.algo.straightSkeleton.implhelpers.{Loop, LoopL}
import utils.math.space.V3


class TestSkeleton extends AnyFlatSpec with should.Matchers {

  def setupFor(vs: (Scalar, Scalar)*): Skeleton = {
    val corners: Seq[Corner] = vs.map((x, y) => new Corner(x, y))
    val speed1 = new Machine(Math.PI / 4)

    val edges = corners
      .asCyclicPairs
      .map { case (c1, c2) => new Edge(c1, c2) }
      .toSeq

    val loop1 = new Loop[Edge](edges: _ *)
    val skel = new Skeleton(loop1.singleton, true)
    skel.skeleton()
    skel
  }

  def dumpFaces(s: Skeleton): Seq[Seq[V3]] =
    s.output.faces.values.toSeq
      .map(_.points.iterator.flatMap(_.iterator).toSeq)


  "StraightSkeleton" should "work for triangle" in {
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
  }

}

