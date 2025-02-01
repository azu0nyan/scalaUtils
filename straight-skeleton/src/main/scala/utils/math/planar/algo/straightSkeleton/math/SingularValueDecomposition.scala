package utils.math.planar.algo.straightSkeleton.math

import scala.util.control.Breaks
import scala.util.control.Breaks.{break, breakable}


/** Singular Value Decomposition.
 * <P>
 * For an m-by-n matrix A with m >= n, the singular value decomposition is
 * an m-by-n orthogonal matrix U, an n-by-n diagonal matrix S, and
 * an n-by-n orthogonal matrix V so that A = U*S*V'.
 * <P>
 * The singular values, sigma[k] = S[k][k], are ordered so that
 * sigma[0] >= sigma[1] >= ... >= sigma[n-1].
 * <P>
 * The singular value decompostion always exists, so the constructor will
 * never fail.  The matrix condition number and the effective numerical
 * rank can be computed from this decomposition.
 *
 * /** Construct the singular value decomposition
 * *
 * * @param A Rectangular matrix
 * * @return Structure to access U, S and V.
 * */
 * /* ------------------------
 * Constructor
 * * ------------------------ */
 *
 */
class SingularValueDecomposition(Arg: Matrix) {
  // Derived from LINPACK code.
  // Initialize.


  val A = Arg.getArrayCopy
  val m = Arg.getRowDimension
  val n = Arg.getColumnDimension
  /* Apparently the failing cases are only a proper subset of (m<n),
     so let's not throw error.  Correct fix to come later?
        if (m<n) {
      throw new IllegalArgumentException("Jama SVD only works for m >= n"); }
        */
  val nu = Math.min(m, n)
  val s = new Array[Double](Math.min(m + 1, n))
  val U = Array.ofDim[Double](m, nu)
  val V = Array.ofDim[Double](n, n)
  val e = new Array[Double](n)
  val work = new Array[Double](m)
  val wantu = true
  val wantv = true
  // Reduce A to bidiagonal form, storing the diagonal elements
  // in s and the super-diagonal elements in e.
  val nct = Math.min(m - 1, n)
  val nrt = Math.max(0, Math.min(n - 2, m))
  for (k <- 0 until Math.max(nct, nrt)) {
    if (k < nct) {
      // Compute the transformation for the k-th column and
      // place the k-th diagonal in s[k].
      // Compute 2-norm of k-th column without under/overflow.
      s(k) = 0
      for (i <- k until m) {
        s(k) = Maths.hypot(s(k), A(i)(k))
      }
      if (s(k) != 0.0) {
        if (A(k)(k) < 0.0) s(k) = -s(k)
        for (i <- k until m) {
          A(i)(k) /= s(k)
        }
        A(k)(k) += 1.0
      }
      s(k) = -s(k)
    }
    for (j <- k + 1 until n) {
      if ((k < nct) && (s(k) != 0.0)) {
        // Apply the transformation.
        var t = 0.0
        for (i <- k until m) {
          t += A(i)(k) * A(i)(j)
        }
        t = -t / A(k)(k)
        for (i <- k until m) {
          A(i)(j) += t * A(i)(k)
        }
      }
      // Place the k-th row of A into e for the
      // subsequent calculation of the row transformation.
      e(j) = A(k)(j)
    }
    if (wantu && (k < nct)) {
      // Place the transformation in U for subsequent back
      // multiplication.
      for (i <- k until m) {
        U(i)(k) = A(i)(k)
      }
    }
    if (k < nrt) {
      // Compute the k-th row transformation and place the
      // k-th super-diagonal in e[k].
      // Compute 2-norm without under/overflow.
      e(k) = 0
      for (i <- k + 1 until n) {
        e(k) = Maths.hypot(e(k), e(i))
      }
      if (e(k) != 0.0) {
        if (e(k + 1) < 0.0) e(k) = -e(k)
        for (i <- k + 1 until n) {
          e(i) /= e(k)
        }
        e(k + 1) += 1.0
      }
      e(k) = -e(k)
      if ((k + 1 < m) && (e(k) != 0.0)) {
        // Apply the transformation.
        for (i <- k + 1 until m) {
          work(i) = 0.0
        }
        for (j <- k + 1 until n) {
          for (i <- k + 1 until m) {
            work(i) += e(j) * A(i)(j)
          }
        }
        for (j <- k + 1 until n) {
          val t = -e(j) / e(k + 1)
          for (i <- k + 1 until m) {
            A(i)(j) += t * work(i)
          }
        }
      }
      if (wantv) {
        // Place the transformation in V for subsequent
        // back multiplication.
        for (i <- k + 1 until n) {
          V(i)(k) = e(i)
        }
      }
    }
  }
  // Set up the final bidiagonal matrix or order p.
  var p = Math.min(n, m + 1)
  if (nct < n) s(nct) = A(nct)(nct)
  if (m < p) s(p - 1) = 0.0
  if (nrt + 1 < p) e(nrt) = A(nrt)(p - 1)
  e(p - 1) = 0.0
  // If required, generate U.
  if (wantu) {
    for (j <- nct until nu) {
      for (i <- 0 until m) {
        U(i)(j) = 0.0
      }
      U(j)(j) = 1.0
    }
    for (k <- nct - 1 to 0 by -1) {
      if (s(k) != 0.0) {
        for (j <- k + 1 until nu) {
          var t = 0.0
          for (i <- k until m) {
            t += U(i)(k) * U(i)(j)
          }
          t = -t / U(k)(k)
          for (i <- k until m) {
            U(i)(j) += t * U(i)(k)
          }
        }
        for (i <- k until m) {
          U(i)(k) = -U(i)(k)
        }
        U(k)(k) = 1.0 + U(k)(k)
        for (i <- 0 until k - 1) {
          U(i)(k) = 0.0
        }
      }
      else {
        for (i <- 0 until m) {
          U(i)(k) = 0.0
        }
        U(k)(k) = 1.0
      }
    }
  }
  // If required, generate V.
  if (wantv) for (k <- n - 1 to 0 by -1) {
    if ((k < nrt) && (e(k) != 0.0)) for (j <- k + 1 until nu) {
      var t = 0.0
      for (i <- k + 1 until n) {
        t += V(i)(k) * V(i)(j)
      }
      t = -t / V(k + 1)(k)
      for (i <- k + 1 until n) {
        V(i)(j) += t * V(i)(k)
      }
    }
    for (i <- 0 until n) {
      V(i)(k) = 0.0
    }
    V(k)(k) = 1.0
  }
  // Main iteration loop for the singular values.
  val pp = p - 1
  var iter = 0
  val eps = Math.pow(2.0, -52.0)
  val tiny = Math.pow(2.0, -966.0)
  while (p > 0) {
    var k = 0
    var kase = 0
    // Here is where a test for too many iterations would go.
    // This section of the program inspects for
    // negligible elements in the s and e arrays.  On
    // completion the variables kase and k are set as follows.
    // kase = 1     if s(p) and e[k-1] are negligible and k<p
    // kase = 2     if s(k) is negligible and k<p
    // kase = 3     if e[k-1] is negligible, k<p, and
    //              s(k), ..., s(p) are not negligible (qr step).
    // kase = 4     if e(p-1) is negligible (convergence).
    k = p - 2
    breakable {
      while (k >= -1) {
        if (k == -1) break
        if (Math.abs(e(k)) <= tiny + eps * (Math.abs(s(k)) + Math.abs(s(k + 1)))) {
          e(k) = 0.0
          break

        }

        k -= 1
      }
    }
    if (k == p - 2) kase = 4
    else {
      var ks = 0
      ks = p - 1

      breakable {
        while (ks >= k) {
          if (ks == k) break
          val t =
            (if (ks != p) Math.abs(e(ks)) else 0.0) +
              (if (ks != k + 1) Math.abs(e(ks - 1)) else 0.0)

          if (Math.abs(s(ks)) <= tiny + eps * t) {
            s(ks) = 0.0
            break 
          }
          ks -= 1
        }
      }
      if (ks == k) kase = 3
      else if (ks == p - 1) kase = 1
      else {
        kase = 2
        k = ks
      }
    }
    k += 1
    // Perform the task indicated by kase.
    kase match {
      // Deflate negligible s(p).
      case 1 =>
        var f = e(p - 2)
        e(p - 2) = 0.0
        for (j <- p - 2 to k by -1) {
          var t = Maths.hypot(s(j), f)
          val cs = s(j) / t
          val sn = f / t
          s(j) = t
          if (j != k) {
            f = -sn * e(j - 1)
            e(j - 1) = cs * e(j - 1)
          }
          if (wantv) for (i <- 0 until n) {
            t = cs * V(i)(j) + sn * V(i)(p - 1)
            V(i)(p - 1) = -sn * V(i)(j) + cs * V(i)(p - 1)
            V(i)(j) = t
          }
        }


      // Split at negligible s(k).
      case 2 =>
        var f = e(k - 1)
        e(k - 1) = 0.0
        for (j <- k until p) {
          var t = Maths.hypot(s(j), f)
          val cs = s(j) / t
          val sn = f / t
          s(j) = t
          f = -sn * e(j)
          e(j) = cs * e(j)
          if (wantu) for (i <- 0 until m) {
            t = cs * U(i)(j) + sn * U(i)(k - 1)
            U(i)(k - 1) = -sn * U(i)(j) + cs * U(i)(k - 1)
            U(i)(j) = t
          }
        }


      // Perform one qr step.
      case 3 =>

        // Calculate the shift.
        val scale = Math.max(Math.max(Math.max(Math.max(Math.abs(s(p - 1)), Math.abs(s(p - 2))), Math.abs(e(p - 2))), Math.abs(s(k))), Math.abs(e(k)))
        val sp = s(p - 1) / scale
        val spm1 = s(p - 2) / scale
        val epm1 = e(p - 2) / scale
        val sk = s(k) / scale
        val ek = e(k) / scale
        val b = ((spm1 + sp) * (spm1 - sp) + epm1 * epm1) / 2.0
        val c = (sp * epm1) * (sp * epm1)
        var shift = 0.0
        if ((b != 0.0) | (c != 0.0)) {
          shift = Math.sqrt(b * b + c)
          if (b < 0.0) shift = -shift
          shift = c / (b + shift)
        }
        var f = (sk + sp) * (sk - sp) + shift
        var g = sk * ek
        // Chase zeros.
        for (j <- k until p - 1) {
          var t = Maths.hypot(f, g)
          var cs = f / t
          var sn = g / t
          if (j != k) e(j - 1) = t
          f = cs * s(j) + sn * e(j)
          e(j) = cs * e(j) - sn * s(j)
          g = sn * s(j + 1)
          s(j + 1) = cs * s(j + 1)
          if (wantv) for (i <- 0 until n) {
            t = cs * V(i)(j) + sn * V(i)(j + 1)
            V(i)(j + 1) = -sn * V(i)(j) + cs * V(i)(j + 1)
            V(i)(j) = t
          }
          t = Maths.hypot(f, g)
          cs = f / t
          sn = g / t
          s(j) = t
          f = cs * e(j) + sn * s(j + 1)
          s(j + 1) = -sn * e(j) + cs * s(j + 1)
          g = sn * e(j + 1)
          e(j + 1) = cs * e(j + 1)
          if (wantu && (j < m - 1)) for (i <- 0 until m) {
            t = cs * U(i)(j) + sn * U(i)(j + 1)
            U(i)(j + 1) = -sn * U(i)(j) + cs * U(i)(j + 1)
            U(i)(j) = t
          }
        }
        e(p - 2) = f
        iter = iter + 1


      // Convergence.
      case 4 =>

        // Make the singular values positive.
        if (s(k) <= 0.0) {
          s(k) = if (s(k) < 0.0) -(s(k))
          else 0.0
          if (wantv) for (i <- 0 to pp) {
            V(i)(k) = -V(i)(k)
          }
        }
        // Order the singular values.
        breakable {
          while (k < pp) {
            if (s(k) >= s(k + 1)) break //todo: break is not supported
            var t = s(k)
            s(k) = s(k + 1)
            s(k + 1) = t
            if (wantv && (k < n - 1)) for (i <- 0 until n) {
              t = V(i)(k + 1)
              V(i)(k + 1) = V(i)(k)
              V(i)(k) = t
            }
            if (wantu && (k < m - 1)) for (i <- 0 until m) {
              t = U(i)(k + 1)
              U(i)(k + 1) = U(i)(k)
              U(i)(k) = t
            }
            k += 1
          }
        }
        iter = 0
        p -= 1
    }
  }

  /** Arrays for internal storage of U and V.
   *
   * @serial internal storage of U.
   * @serial internal storage of V.
   */


  /** Return the left singular vectors
   *
   * @return U
   */
  /* ------------------------
     Public Methods
   * ------------------------ */
  def getU = new Matrix(U, m, Math.min(m + 1, n))
  /** Return the right singular vectors
   *
   * @return V
   */
  def getV = new Matrix(V, n, n)
  /** Return the one-dimensional array of singular values
   *
   * @return diagonal of S.
   */
  def getSingularValues = s
  /** Return the diagonal matrix of singular values
   *
   * @return S
   */
  def getS = {
    val X = new Matrix(n, n)
    val S = X.getArray
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        S(i)(j) = 0.0
      }
      S(i)(i) = this.s(i)
    }
    X
  }
  /** Two norm
   *
   * @return max(S)
   */
  def norm2 = s(0)
  /** Two norm condition number
   *
   * @return max(S)/min(S)
   */
  def cond = s(0) / s(Math.min(m, n) - 1)
  /** Effective numerical matrix rank
   *
   * @return Number of nonnegligible singular values.
   */
  def rank = {
    val eps = Math.pow(2.0, -52.0)
    val tol = Math.max(m, n) * s(0) * eps
    var r = 0
    for (i <- 0 until s.length) {
      if (s(i) > tol) r += 1
    }
    r
  }
}