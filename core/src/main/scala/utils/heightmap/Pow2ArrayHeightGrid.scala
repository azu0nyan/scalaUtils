package utils.heightmap

import utils.datastructures.IntV2
import utils.math.*

class Pow2ArrayHeightGrid(val size2Pow: Int) extends HeightGrid {



  def this(h:HeightGrid) = {
    this(log2(h.resolution.x).toInt)
    fillFrom(h)
  }

  override def apply(flat:Int):Scalar = heightAt(flat)
  /** Grid size, indices will be from (0, 0) to resolution - (1, 1) */
  val size: Int = 1 << size2Pow
  val yMask: Int = (for (i <- 0 until size2Pow) yield 1 << i).reduce(_ | _)
  @inline override final def fromFlatIndex(index: Int): IntV2 = IntV2(index >> size2Pow, index & yMask)
  @inline override final def toFlatHeightIndex(index: IntV2): Int = (index.x << size2Pow) | index.y

  override val resolution: IntV2 = IntV2(size)


  val array: Array[Scalar] = Array.ofDim(valuesCount)
  def fillFrom(from: HeightGrid): Unit = {
    for (i <- 0 until resolution.area) {
      array(i) = from.valueAt(fromFlatIndex(i))
    }
  }
  /** do not call directly, use "valueAt" for safety check */
  override def valueAtUnsafe(pos: IntV2): Scalar = array(toFlatIndex(pos))

  //fast ops


  @inline final def setValue(pos: Int, newValue: Scalar): Unit = {
    array(pos) = newValue
  }

  @inline final def modifyValue(pos: Int, newValue: Scalar): Unit = {
    array(pos) += newValue
  }

  @inline final def heightAt(pos: Int): Scalar = array(pos)


  def setValue(pos: IntV2, newValue: Scalar): Unit = {
    if (validIndex(pos)) {
      setValue(toFlatIndex(pos), newValue)
    }
  }

  def modifyValue(pos: IntV2, delta: Scalar): Unit = {
    if (validIndex(pos)) {
      array(toFlatIndex(pos)) += delta
    }
  }



  /** xxxxxxxxxxxxxxxxxyyyyyyyyyyyyyyy **/
  @inline final def x(i:Int) :Int = i >> size2Pow
  @inline final def y(i:Int) :Int  = i & yMask
  @inline final def left(i: Int): Int = i - size
  @inline final def right(i: Int): Int = i + size
  @inline final def top(i: Int): Int = i - 1
  @inline final def topLeft(i: Int): Int = i - 1 - size
  @inline final def topRight(i: Int): Int = i - 1 + size
  @inline final def bot(i: Int): Int = i + 1
  @inline final def botLeft(i: Int): Int = i + 1 - size
  @inline final def botRight(i: Int): Int = i + 1 + size


  /** ignoring case with 1x1 grid, assuming every cell have at least 2 neighbours */
  @inline def flatNeighboursXY(i: Int): Array[Int] = {
    val x = i >> size2Pow
    val y = i & yMask
    if (x > 0) { //have left
      if (x < size - 1) { //have left and right
        if (y > 0) { //have top
          if (y < size - 1) { //have top and bot
            Array(left(i), right(i), top(i), bot(i))
          } else { // only top
            Array(left(i), right(i), top(i))
          }
        } else { //no top => have bot
          Array(left(i), right(i), bot(i))
        }
      } else { // only left
        if (y > 0) { //have top
          if (y < size - 1) { //have top and bot
            Array(left(i),  top(i), bot(i))
          } else { // only top
            Array(left(i),  top(i))
          }
        } else { //no top => have bot
          Array(left(i),  bot(i))
        }
      }
    } else { //no left => have right
      if (y > 0) { //have top
        if (y < size - 1) { //have top and bot
          Array( right(i), top(i), bot(i))
        } else { // only top
          Array( right(i), top(i))
        }
      } else { //no top => have bot
        Array( right(i), bot(i))
      }
    }
  }


  @inline def isNeighboursDiagonal(f:Int, s:Int ): Boolean = {
    val xf = f >> size2Pow
    val yf = f & yMask
    val xs = s >> size2Pow
    val ys = s & yMask
    xf == xs || yf == ys
  }

  @inline def flatNeighboursDiag(i: Int): Array[Int] ={
    val x = i >> size2Pow
    val y = i & yMask
    if (x > 0) { //have left
      if (x < size - 1) { //have left and right
        if (y > 0) { //have top
          if (y < size - 1) { //have top and bot
            Array(left(i), right(i), top(i), bot(i), topLeft(i), topRight(i), botLeft(i), botRight(i))
          } else { // only top
            Array(left(i), right(i), top(i),  topLeft(i), topRight(i))
          }
        } else { //no top => have bot
          Array(left(i), right(i), bot(i), botLeft(i), botRight(i))
        }
      } else { // only left
        if (y > 0) { //have top
          if (y < size - 1) { //have top and bot
            Array(left(i),  top(i), bot(i), topLeft(i), botLeft(i))
          } else { // only top
            Array(left(i),  top(i), topLeft(i))
          }
        } else { //no top => have bot
          Array(left(i),  bot(i), botLeft(i))
        }
      }
    } else { //no left => have right
      if (y > 0) { //have top
        if (y < size - 1) { //have top and bot
          Array( right(i), top(i), bot(i), topRight(i), botRight(i))
        } else { // only top
          Array( right(i), top(i), topRight(i))
        }
      } else { //no top => have bot
        Array( right(i), bot(i), botRight(i))
      }
    }
  }
}
