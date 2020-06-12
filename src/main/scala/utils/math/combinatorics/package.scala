package utils.math

import scala.collection.mutable.ArrayBuffer

package object combinatorics {

  val factorials:ArrayBuffer[Long] = new ArrayBuffer[Long]()
  factorials += 1 // 0! = 1
  factorials += 1 // 1! = 1

  def factorial(i:Int):Long = {
    if(factorials.size <= i) {
      for(j <- factorials.size to i){
        factorials += factorials(j - 1) * j
      }
    }
    factorials(i)
  }
}
