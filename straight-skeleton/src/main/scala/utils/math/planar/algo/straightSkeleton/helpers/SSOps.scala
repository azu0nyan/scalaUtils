package utils.math.planar.algo.straightSkeleton.helpers


import utils.datastructures.CircullarOps.{asCyclicPairs, existsWithShifts}
import utils.math.Scalar
import utils.math.planar.V2
import utils.math.planar.algo.straightSkeleton.{Corner, Edge, Machine, Skeleton}
import utils.math.planar.algo.straightSkeleton.implhelpers.{Loop, LoopL}
import utils.math.space.V3


object SSOps {
  def setupFor(vs: (Scalar, Scalar)*): Skeleton = {
    val corners: Seq[Corner] = vs.map((x, y) => new Corner(x, y))
    val speed1 = new Machine(Math.PI / 4)

    val edges = corners
      .asCyclicPairs
      .map { case (c1, c2) => new Edge(c1, c2) }
      .toSeq

    for (e <- edges)
      e.machine = speed1

    val loop1 = new Loop[Edge](edges: _ *)
    val skel = new Skeleton(loop1.singleton, true)
    skel.skeleton()
    skel
  }

  def dumpFaces(s: Skeleton): Seq[Seq[V3]] =
    s.output.faces.values.toSeq
      .map(_.points.iterator.flatMap(_.iterator).toSeq)


  def almostEqWithShifts(s1: Seq[Seq[V3]], s2: Seq[Seq[V3]]): Boolean =
    s1.existsWithShifts(s1 =>
      s1.zip(s2).forall((s1s, s2s) =>
        s1s.existsWithShifts(s1ss =>
          s1ss.zip(s2s).forall(_ ~= _)
        )
      )
    )


}
