package utils.math.linalg

import utils.math.linalg.Matrix.SquareMatrix
import utils.math.{Scalar, abs}
import utils.math._

object MatrixInverse {

  /** swapping rows matrix should be at left SR * M, row1 != row2 */
  def swapRows(row1: Int, row2: Int, _size: Int): SquareMatrix = new SquareMatrix {
    override def size: Int = _size
    override def valueAt(row: Int, column: Int): Scalar =
      if (row == row1 && column == row2 || row == row2 && column == row1) 1d
      else if (row == column && (row == row1 || row == row2)) 0d
      else if (row == column) 1d
      else 0d
  }

  /** multiplying row by coeff */
  def mullRow(rowToMultiply: Int, coeff: Scalar, _size: Int): SquareMatrix = new SquareMatrix {
    override def size: Int = _size
    override def valueAt(row: Int, column: Int): Scalar =
      if (row != column) 0d
      else if (row == rowToMultiply) coeff else 1d
  }

  /** adds rowFrom to rowTo with coeff */
  def sumRow(rowTo: Int, rowFrom: Int, coeff: Scalar, _size: Int): SquareMatrix = new SquareMatrix {
    override def size: Int = _size
    override def valueAt(row: Int, column: Int): Scalar =
      if (row == column) 1d else if (row == rowTo && column == rowFrom) coeff else 0d
  }

  def inverse(m: SquareMatrix): Option[SquareMatrix] = Option.when(m.det != 0)(inverseUnsafeGaussJordanElimination(m))


  /***/
  def inverseUnsafeGaussJordanElimination(m: SquareMatrix): SquareMatrix = {
    //todo optimize with row operations instead of matrix multiplication

    val n = m.size
    var currentMatrix: SquareMatrix = m
    var currentInverse: SquareMatrix = Matrix.id(n)

    for (columnRow <- 0 until n) {
      //find and swap max value in a column to improve numeric stability(as wiki says)
      var maxRow = columnRow
      for (i <- (columnRow  + 1) until n) {
        if (abs(currentMatrix(i, columnRow)) > abs(currentMatrix(maxRow, columnRow))) {
          maxRow = i
        }
      }
      if (maxRow != columnRow) {
        val op = swapRows(columnRow, maxRow, n)
        currentMatrix = op *** currentMatrix
        currentInverse = op *** currentInverse
      }

      val currentValue = currentMatrix(columnRow, columnRow)
      if (currentValue !~= 0d) {
        for (row <- 0 until n if row != columnRow) {
          val toSubtract = currentMatrix(row, columnRow)
          if (toSubtract !~= 0d) {
            val coeff = -toSubtract / currentValue
            val op = sumRow(row, columnRow, coeff, n)
            currentMatrix = op *** currentMatrix
            currentInverse = op *** currentInverse
          }
        }
        if (currentValue !~= 1d) {
          val coeff = 1d / currentValue
          val op = mullRow(columnRow, coeff, n)
          currentMatrix = op *** currentMatrix
          currentInverse = op *** currentInverse
        }
      }
    }
    return currentInverse
  }

}
