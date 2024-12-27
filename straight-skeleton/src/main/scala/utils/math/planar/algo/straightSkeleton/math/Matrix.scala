package utils.math.planar.algo.straightSkeleton.math


class Matrix extends Cloneable with Serializable {
  private var A: Array[Array[Double]] = null
  private var m = 0
  private var n = 0
  def this(var1: Array[Array[Double]]) = {
    this()
    this.m = var1.length
    this.n = var1(0).length
    for (var2 <- 0 until this.m) {
      if (var1(var2).length != this.n) throw new IllegalArgumentException("All rows must have the same length.")
    }
    this.A = var1
  }

  def this(var1: Int, var2: Int) = {
    this()
    this.m = var1
    this.n = var2
    this.A = Array.ofDim[Double](var1, var2)
  }
  def this(var1: Array[Array[Double]], var2: Int, var3: Int) = {
    this()
    this.A = var1
    this.m = var2
    this.n = var3
  }

  def this(var1: Array[Double], var2: Int) = {
    this()
    m = var2
    n = if (var2 != 0) var1.length / var2 else 0

    if (var2 * this.n != var1.length) throw new IllegalArgumentException("Array length must be a multiple of m.")
    else {
      A = Array.ofDim[Double](var2, this.n)
      for (var3 <- 0 until var2) {
        for (var4 <- 0 until this.n) {
          this.A(var3)(var4) = var1(var3 + var4 * var2)
        }
      }
    }
  }


  def getMatrix(var1: Array[Int], var2: Int, var3: Int) = {
    val var4 = new Matrix(var1.length, var3 - var2 + 1)
    val var5 = var4.getArray
    try {
      for (var6 <- 0 until var1.length) {
        for (var7 <- var2 to var3) {
          var5(var6)(var7 - var2) = this.A(var1(var6))(var7)
        }
      }
      var4
    } catch {
      case var8: ArrayIndexOutOfBoundsException =>
        throw new ArrayIndexOutOfBoundsException("Submatrix indices")
    }
  }

  def get(var1: Int, var2: Int) = this.A(var1)(var2)

  def getArray = this.A

  def solve(var1: Matrix) =
    if (m == n) new LUDecomposition(this).solve(var1)
    else ???

  def getArrayCopy = {
    val var1 = Array.ofDim[Double](m, n)
    for (var2 <- 0 until m) {
      for (var3 <- 0 until n) {
        var1(var2)(var3) = A(var2)(var3)
      }
    }
    var1
  }
  def getRowDimension = m

  def getColumnDimension = n

  def rank = new SingularValueDecomposition(this).rank
}

object Matrix3d {
  def make(m00: Double, m01: Double, m02: Double, m10: Double, m11: Double, m12: Double, m20: Double, m21: Double, m22: Double) =
    new Matrix(
      Array(
        Array(m00, m01, m02),
        Array(m10, m11, m12),
        Array(m20, m21, m22)
      )
    )
}