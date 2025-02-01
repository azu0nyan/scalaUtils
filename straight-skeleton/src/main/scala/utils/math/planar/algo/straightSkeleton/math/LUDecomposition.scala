package utils.math.planar.algo.straightSkeleton.math

import utils.math.planar.algo.straightSkeleton.math.Matrix


/** LU Decomposition.
 * <P>
 * For an m-by-n matrix A with m >= n, the LU decomposition is an m-by-n
 * unit lower triangular matrix L, an n-by-n upper triangular matrix U,
 * and a permutation vector piv of length m so that A(piv,:) = L*U.
 * If m < n, then L is m-by-m and U is m-by-n.
 * <P>
 * The LU decompostion with pivoting always exists, even if the matrix is
 * singular, so the constructor will never fail.  The primary use of the
 * LU decomposition is in the solution of square systems of simultaneous
 * linear equations.  This will fail if isNonsingular() returns false.
 *
 * LU Decomposition
 * @param A Rectangular matrix
 * @return Structure to access L, U and piv.
 */
class LUDecomposition(A: Matrix){
  /* ------------------------
      Class variables
    * ------------------------ */
  var LU: Array[Array[Double]] = A.getArrayCopy
  /** Row and column dimensions, and pivot sign.
   *
   * @serial column dimension.
   * @serial row dimension.
   * @serial pivot sign.
   */
  var m = A.getRowDimension
  var n = A.getColumnDimension
  var pivsign = 1
  /** Internal storage of pivot vector.
   *
   * @serial pivot vector.
   */
   var piv: Array[Int] =  new Array[Int](m)
  // Use a "left-looking", dot-product, Crout/Doolittle algorithm.
  for (i <- 0 until m) {
    piv(i) = i
  }
  var LUrowi: Array[Double] = null
  val LUcolj = new Array[Double](m)
  // Outer loop.
  for (j <- 0 until n) {
    // Make a copy of the j-th column to localize references.
    for (i <- 0 until m) {
      LUcolj(i) = LU(i)(j)
    }
    // Apply previous transformations.
    for (i <- 0 until m) {
      LUrowi = LU(i)
      // Most of the time is spent in the following dot product.
      val kmax = Math.min(i, j)
      var s = 0.0
      for (k <- 0 until kmax) {
        s += LUrowi(k) * LUcolj(k)
      }
      LUcolj(i) -= s
      LUrowi(j) = LUcolj(i) 
    }
    // Find pivot and exchange if necessary.
    var p = j
    for (i <- j + 1 until m) {
      if (Math.abs(LUcolj(i)) > Math.abs(LUcolj(p))) p = i
    }
    if (p != j) {
      for (k <- 0 until n) {
        val t = LU(p)(k)
        LU(p)(k) = LU(j)(k)
        LU(j)(k) = t
      }
      val k = piv(p)
      piv(p) = piv(j)
      piv(j) = k
      pivsign = -pivsign
    }
    // Compute multipliers.
    if (j < m & LU(j)(j) != 0.0) for (i <- j + 1 until m) {
      LU(i)(j) /= LU(j)(j)
    }
  }
  /** Array for internal storage of decomposition.
   * @serial internal array storage.
   */

 
  /* ------------------------
     Public Methods
   * ------------------------ */
  def isNonsingular: Boolean = {
    for (j <- 0 until n) {
      if (LU(j)(j) == 0) return false
    }
    true
  }
  /** Return lower triangular factor
   * @return L
   */
  def getL = {
    val X = new Matrix(m, n)
    val L = X.getArray
    for (i <- 0 until m) {
      for (j <- 0 until n) {
        if (i > j) L(i)(j) = LU(i)(j)
        else if (i == j) L(i)(j) = 1.0
        else L(i)(j) = 0.0
      }
    }
    X
  }
  /** Return upper triangular factor
   * @return U
   */
  def getU = {
    val X = new Matrix(n, n)
    val U = X.getArray
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        if (i <= j) U(i)(j) = LU(i)(j)
        else U(i)(j) = 0.0
      }
    }
    X
  }
  /** Return pivot permutation vector
   * @return piv
   */
  def getPivot = {
    val p = new Array[Int](m)
    for (i <- 0 until m) {
      p(i) = piv(i)
    }
    p
  }
  /** Return pivot permutation vector as a one-dimensional double array
   * @return (double) piv
   */
  def getDoublePivot = {
    val vals = new Array[Double](m)
    for (i <- 0 until m) {
      vals(i) = piv(i).toDouble
    }
    vals
  }
  /** Determinant
   * @return det(A)
   * @exception IllegalArgumentException  Matrix must be square
   */
  def det = {
    if (m != n) throw new IllegalArgumentException("Matrix must be square.")
    var d = pivsign.toDouble
    for (j <- 0 until n) {
      d *= LU(j)(j)
    }
    d
  }
  /** Solve A*X = B
   * @param B A Matrix with as many rows as A and any number of columns.
   * @return X so that L*U*X = B(piv,:)
   * @exception IllegalArgumentException Matrix row dimensions must agree.
   * @exception RuntimeException  Matrix is singular.
   */
  def solve(B: Matrix) = {
    if (B.getRowDimension != m) throw new IllegalArgumentException("Matrix row dimensions must agree.")
    if (!this.isNonsingular) throw new RuntimeException("Matrix is singular.")
    // Copy right hand side with pivoting
    val nx = B.getColumnDimension
    val Xmat = B.getMatrix(piv, 0, nx - 1)
    val X = Xmat.getArray
    // Solve L*Y = B(piv,:)
    for (k <- 0 until n) {
      for (i <- k + 1 until n) {
        for (j <- 0 until nx) {
          X(i)(j) -= X(k)(j) * LU(i)(k)
        }
      }
    }
    // Solve U*X = Y;
    for (k <- n - 1 to 0 by -1) {
      for (j <- 0 until nx) {
        X(k)(j) /= LU(k)(k)
      }
      for (i <- 0 until k) {
        for (j <- 0 until nx) {
          X(i)(j) -= X(k)(j) * LU(i)(k)
        }
      }
    }
    Xmat
  }
}