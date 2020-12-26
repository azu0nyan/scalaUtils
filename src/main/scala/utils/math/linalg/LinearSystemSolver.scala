package utils.math.linalg

import utils.math.linalg.Matrix.SquareMatrix
import utils.math._

import scala.collection.immutable.ArraySeq

object LinearSystemSolver {

  /**
   * M * x = b   <br>
   *
   * M^-1^ * M * x = M^-1^ * b  <br>
   * x = M^-1^ * b
   *
   * @param m
   */
  def solveUnsafe(m: SquareMatrix, b: IndexedSeq[Scalar]): IndexedSeq[Scalar] = {
    val minv = m.inverseUnsafe
    (minv ** b).toIndexedSeq
  }
  /**
   * from https://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm <br>
   * a,,0,, == 0 && c,,n-1,, == 0  <br>
   * | b,,0,, c,,0,, ...............0| |x,,0,,| |d,,0,,|<br>
   * | a,,1,, b,,1,, c,,1,, ..........0| |x,,1,,| |d,,1,,|<br>
   * | 0 a,,2,, b,,2,, c,,2,, .......0| |x,,2,,| |d,,2,,|<br>
   * |...........................| |...|=|...|<br>
   * |....................c,,n-2,,| |...|  |...|<br>
   * |...............a,,n-1,, c,,n,,| |x,,n-1,,| |d,,n-1,,|<br>
   *
   * @param a
   * @param b
   * @param c
   * @param d
   * @return
   */
  def solveTridiagonal(a: IndexedSeq[Scalar], b: IndexedSeq[Scalar], c: IndexedSeq[Scalar], d: IndexedSeq[Scalar]): IndexedSeq[Scalar] = {
    val n = a.size
    val bs: Array[Scalar] = b.toArray
    val ds: Array[Scalar] = d.toArray

    for (i <- 1 until n) {
      val w = a(i) / bs(i - 1)
      bs(i) = bs(i) - w * c(i - 1)
      ds(i) = ds(i) - w * ds(i - 1)
    }
    val xs: Array[Scalar] = new Array[Scalar](n)
    xs(n - 1) = ds(n - 1) / bs(n - 1)
    for (i <- (n - 2) to 0 by -1) {
      xs(i) = (ds(i) - c(i) * xs(i + 1)) / bs(i)
    }
    return ArraySeq.unsafeWrapArray(xs)
  }
  //matrix nx(n+1) where last column is fre coeffs
  def solve(system: Seq[Seq[Scalar]]): Option[Seq[Scalar]] = {
    val n = system.size
    val arr = Array.tabulate(n, n + 1)((x, y) => system(x)(y))

    def swapRows(i: Int, j: Int): Unit = {
      val tmp = arr(j)
      arr(j) = arr(i)
      arr(i) = tmp
    }
    def mullRow(i: Int, coeff: Scalar): Unit = {
      for (j <- 0 until (n + 1)) arr(i)(j) *= coeff
    }
    def sumRow(from: Int, to: Int, coeff: Scalar): Unit = {
      for (j <- 0 until (n + 1)) arr(to)(j) += arr(from)(j) * coeff
    }

    for (i <- 0 until n) {
      //find row with 0
      if (arr(i)(i) == 0) {
        var cur = i + 1
        var found = false
        while (cur < n && !found) {
          if (arr(cur)(i) != 0) {
            found = true
            swapRows(i, cur)
          } else cur += 1
        }
        if (!found) return None
      }
      mullRow(i, 1.0 / arr(i)(i))
      for (to <- 0 until n if to != i) {
        sumRow(i, to, -arr(to)( i))
      }
    }
    Some(arr.map(_.last).toSeq)
  }

}
