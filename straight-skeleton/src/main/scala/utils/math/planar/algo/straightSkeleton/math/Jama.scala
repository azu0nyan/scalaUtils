package utils.math.planar.algo.straightSkeleton.math

import utils.math.space.V3

object Jama {

  def solve(A: Matrix, offset: V3): Option[V3] = {
    val am = A
    val bs = Array[Double](offset.x, offset.y, offset.z)
    val bm = new Matrix(bs, 3)
    val out = am.solve(bm).getArray
    val d = offset.x * out(0)(0) + offset.y * out(1)(0) + offset.z * out(2)(0)
    Option.when(am.rank == 3)(V3(out(0)(0), out(1)(0), out(2)(0)))
  }
}
