package utils.math.linalg

import utils.datastructures.IntV2
import utils.math.Scalar
import utils.math._

object Matrix {

  //  def inverse4(m: MatrixArrayImpl): Matrix = MatrixArrayImpl(MatrixUtils.inverse4(m.data))

  def diagonal(a: Array[Scalar]): Matrix = MatrixArrayImpl(MatrixUtils.scaleMatrix(a))

  def id(i: Int): SquareMatrix = new SquareMatrix {
    override def size: Int = i
    override def valueAt(row: Int, column: Int): Scalar = if (row == column) 1 else 0
  }

  def fillByRule(r: Int, c: Int, rule: (Int, Int) => Scalar): Matrix = new Matrix {
    val data: IndexedSeq[Scalar] =
      for (i <- 0 until r;
           j <- 0 until c) yield rule(i, j)
    override val rows: Int = r
    override val columns: Int = c
    override def valueAt(row: Int, column: Int): Scalar = data(row * columns + column)
  }

  def fillByRuleSquare(_size: Int, rule: (Int, Int) => Scalar): SquareMatrix = new SquareMatrix {
    val data: IndexedSeq[Scalar] =
      for (i <- 0 until _size;
           j <- 0 until _size) yield rule(i, j)
    override def size: Int = _size
    override def valueAt(row: Int, column: Int): Scalar = data(row * columns + column)
  }

  def squareFromSeqs(rowsColumns: Seq[Seq[Scalar]]): SquareMatrix = new SquareMatrix {
    override def size: Int = rowsColumns.size
    override def valueAt(row: Int, column: Int): Scalar = rowsColumns(row)(column)
  }


  trait SquareMatrix extends Matrix {
    def size: Int

    override final def columns: Int = size
    override final def rows: Int = size

    def det: Scalar = ???

    def inverse: Option[Matrix] = MatrixInverse.inverse(this)
    def inverseUnsafe: SquareMatrix = MatrixInverse.inverseUnsafeGaussJordanElimination(this)

    /** Generic multiplication of square matrix  of any size, for optimized versions use "*" if implementation provided */
    def ***(ot: SquareMatrix): SquareMatrix = fillByRuleSquare(size, (r, c) => {
      var sum: Scalar = 0
      var i: Int = 0
      while (i < columns) {
        sum += valueAt(r, i) * ot.valueAt(i, c)
        i += 1
      }
      sum
    })
  }

  trait Matrix {
    def rows: Int

    def columns: Int

    def transposed: Matrix = new Matrix {
      override def rows: Int = columns
      override def columns: Int = rows
      override def valueAt(row: Int, column: Int): Scalar = Matrix.this.valueAt(column, row)
    }

    def valueAt(row: Int, column: Int): Scalar
    def apply(r: Int, c: Int): Scalar = valueAt(r, c)
    def apply(rc: (Int, Int)): Scalar = valueAt(rc._1, rc._2)
    def apply(rc: IntV2): Scalar = valueAt(rc.x, rc.y)


    /** Generic multiplication of matrix  of any size, for optimized versions use "*" if implementation provided */
    def **(ot: Matrix): Matrix = fillByRule(rows, ot.columns, (r, c) => {
      var sum: Scalar = 0
      var i: Int = 0
      while (i < columns) {
        sum += valueAt(r, i) * ot.valueAt(i, c)
        i += 1
      }
      sum
    })

    /** generic multiplication of any matrix by vector todo faster */
    def **(vector: Seq[Scalar]): Seq[Scalar] = (this ** fillByRule(vector.size, 1, (r, _) => vector(r))).as1dArray.toSeq

    def **(sc: Scalar): Matrix = fillByRule(rows, columns, valueAt(_, _) * sc)

    /** generic matrix comparision */
    def ====(ot: Matrix): Boolean = if (rows != ot.rows || columns != ot.columns) false else {
      for (
        i <- 0 until rows;
        j <- 0 until columns) {
        if (valueAt(i, j) != ot.valueAt(i, j)) {
          return false
        }
      }
      return true
    }

    /** generic matrix almost equals */
    def ~==(ot: Matrix): Boolean = if (rows != ot.rows || columns != ot.columns) false else {
      var i: Int = 0
      while (i < rows) {
        var j: Int = 0
        while (j < columns) {
          if (this (i, j) !~= ot(j, i)) return true
          j += 1
        }
        i += 1
      }
      return true
    }

    /** modifying array may or may not affect matrix depends on implementation */
    def as2dArray: Array[Array[Scalar]] = {
      val res: Array[Array[Scalar]] = Array.ofDim(rows, columns)
      var i: Int = 0
      while (i < rows) {
        var j: Int = 0
        while (j < columns) {
          res(i)(j) = valueAt(i, j)
          j += 1
        }
        i += 1
      }
      return res
    }

    def as1dArray: Array[Scalar] = {
      val res: Array[Scalar] = new Array[Scalar](rows * columns)

      var i: Int = 0
      while (i < rows) {
        var j: Int = 0
        while (j < columns) {
          res(i * columns + j) = valueAt(i, j)
          j += 1
        }
        i += 1
      }
      return res
    }

    def toMultilineString: String = as2dArray.map(a => s"${a.map(v => f"${f"$v%4.4f".reverse.dropWhile(_ == '0').dropWhile(_ == ',') reverse}%8s").mkString("[", "|", "]")}\n").reduce(_ + _)
  }


  case class MatrixArrayImpl(data: Array[Array[Scalar]]) extends Matrix {

    // def *(ot: Matrix): Matrix = MatrixArrayImpl(MatrixUtils.mullSquareMatrices(this.data, ot.as2dArray))

    override val rows: Int = data.length

    override val columns: Int = data(0).length

    override def valueAt(row: Int, column: Int): Scalar = data(row)(column)

    override def as2dArray: Array[Array[Scalar]] = data
  }

}
