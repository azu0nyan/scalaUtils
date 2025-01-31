package utils

import utils.datastructures.spatial.AARectangle
import utils.math.planar.{Polygon, V2}

import scala.util.Random

object RandomUtils extends RandomUtils

trait RandomUtils {

  def randomInPolygon(polygon: Polygon, retryCount:Int = 1000)(implicit seed:Int):Option[V2] = {
    var x = seed * 2342
    val rect = polygon.aabb
    LazyList.continually{
      x += 1
      randomInRectangle(rect)(x)
    }.take(retryCount).dropWhile(!polygon.contains(_))
  }.headOption
    
  
  
  def randomInRectangle(rect:AARectangle)(implicit seed:Int):V2 =
    randomBetween(rect.min, rect.max)

  def randomBetween(min:V2, max:V2)(implicit seed:Int):V2 = {
    val r = new Random(seed*19433293)
    V2(min.x + r.nextDouble() * (max.x - min.x), min.y + r.nextDouble() * (max.y - min.y))
  }

  def randomInt(fromInc:Int, toUninc:Int)(implicit seed: Int):Int = new Random(seed).nextInt(toUninc - fromInc) + fromInc

  def randomFromSeq[T](seq: Seq[T])(implicit seed: Int): T = seq(new Random(seed).nextInt(seq.size))
}
