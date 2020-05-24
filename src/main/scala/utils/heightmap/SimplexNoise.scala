package utils.heightmap

import utils.datastructures.IntV2
import utils.math.planar.V2
import utils.math._

object SimplexNoise {
  private val grad3 = Array(Array(1, 1, 0), Array(-1, 1, 0), Array(1, -1, 0), Array(-1, -1, 0), Array(1, 0, 1), Array(-1, 0, 1), Array(1, 0, -1), Array(-1, 0, -1), Array(0, 1, 1), Array(0, -1, 1), Array(0, 1, -1), Array(0, -1, -1))
  private val grad4 = Array(Array(0, 1, 1, 1), Array(0, 1, 1, -1), Array(0, 1, -1, 1), Array(0, 1, -1, -1), Array(0, -1, 1, 1), Array(0, -1, 1, -1), Array(0, -1, -1, 1), Array(0, -1, -1, -1), Array(1, 0, 1, 1), Array(1, 0, 1, -1), Array(1, 0, -1, 1), Array(1, 0, -1, -1), Array(-1, 0, 1, 1), Array(-1, 0, 1, -1), Array(-1, 0, -1, 1), Array(-1, 0, -1, -1), Array(1, 1, 0, 1), Array(1, 1, 0, -1), Array(1, -1, 0, 1), Array(1, -1, 0, -1), Array(-1, 1, 0, 1), Array(-1, 1, 0, -1), Array(-1, -1, 0, 1), Array(-1, -1, 0, -1), Array(1, 1, 1, 0), Array(1, 1, -1, 0), Array(1, -1, 1, 0), Array(1, -1, -1, 0), Array(-1, 1, 1, 0), Array(-1, 1, -1, 0), Array(-1, -1, 1, 0), Array(-1, -1, -1, 0))
  private val p = Array(151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233, 7, 225, 140, 36, 103, 30, 69, 142, 8, 99, 37, 240, 21, 10, 23, 190, 6, 148, 247, 120, 234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117, 35, 11, 32, 57, 177, 33, 88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168, 68, 175, 74, 165, 71, 134, 139, 48, 27, 166, 77, 146, 158, 231, 83, 111, 229, 122, 60, 211, 133, 230, 220, 105, 92, 41, 55, 46, 245, 40, 244, 102, 143, 54, 65, 25, 63, 161, 1, 216, 80, 73, 209, 76, 132, 187, 208, 89, 18, 169, 200, 196, 135, 130, 116, 188, 159, 86, 164, 100, 109, 198, 173, 186, 3, 64, 52, 217, 226, 250, 124, 123, 5, 202, 38, 147, 118, 126, 255, 82, 85, 212, 207, 206, 59, 227, 47, 16, 58, 17, 182, 189, 28, 42, 223, 183, 170, 213, 119, 248, 152, 2, 44, 154, 163, 70, 221, 153, 101, 155, 167, 43, 172, 9, 129, 22, 39, 253, 19, 98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104, 218, 246, 97, 228, 251, 34, 242, 193, 238, 210, 144, 12, 191, 179, 162, 241, 81, 51, 145, 235, 249, 14, 239, 107, 49, 192, 214, 31, 181, 199, 106, 157, 184, 84, 204, 176, 115, 121, 50, 45, 127, 4, 150, 254, 138, 236, 205, 93, 222, 114, 67, 29, 24, 72, 243, 141, 128, 195, 78, 66, 215, 61, 156, 180)
  // To remove the need for index wrapping, double the permutation table length
  private val perm = p ++ p

  // A lookup table to traverse the simplex around a given point in 4D.
  // Details can be found where this table is used, in the 4D noise method.
  private val simplex = Array(Array(0, 1, 2, 3), Array(0, 1, 3, 2), Array(0, 0, 0, 0), Array(0, 2, 3, 1), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(1, 2, 3, 0), Array(0, 2, 1, 3), Array(0, 0, 0, 0), Array(0, 3, 1, 2), Array(0, 3, 2, 1), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(1, 3, 2, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(1, 2, 0, 3), Array(0, 0, 0, 0), Array(1, 3, 0, 2), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(2, 3, 0, 1), Array(2, 3, 1, 0), Array(1, 0, 2, 3), Array(1, 0, 3, 2), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(2, 0, 3, 1), Array(0, 0, 0, 0), Array(2, 1, 3, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(2, 0, 1, 3), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(3, 0, 1, 2), Array(3, 0, 2, 1), Array(0, 0, 0, 0), Array(3, 1, 2, 0), Array(2, 1, 0, 3), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(3, 1, 0, 2), Array(0, 0, 0, 0), Array(3, 2, 0, 1), Array(3, 2, 1, 0))

  // This method is a *lot* faster than using (int)Math.floor(x)
  private def fastfloor(x: Double) = if (x > 0) x.toInt
  else x.toInt - 1

  private def dot(g: Array[Int], x: Double, y: Double) = g(0) * x + g(1) * y

  private def dot(g: Array[Int], x: Double, y: Double, z: Double) = g(0) * x + g(1) * y + g(2) * z

  private def dot(g: Array[Int], x: Double, y: Double, z: Double, w: Double) = g(0) * x + g(1) * y + g(2) * z + g(3) * w

  def noise(xin: Float, yin: Double): Double = {
    var n0 = .0
    var n1 = .0
    var n2 = .0 // Noise contributions from the three corners
    // Skew the input space to determine which simplex cell we're in
    val F2 = 0.5 * (Math.sqrt(3.0) - 1.0)
    val s = (xin + yin) * F2 // Hairy factor for 2D
    val i = fastfloor(xin + s)
    val j = fastfloor(yin + s)
    val G2 = (3.0 - Math.sqrt(3.0)) / 6.0
    val t = (i + j) * G2
    val X0 = i - t // Unskew the cell origin back to (x,y) space
    val Y0 = j - t
    val x0 = xin - X0 // The x,y distances from the cell origin
    val y0 = yin - Y0
    // For the 2D case, the simplex shape is an equilateral triangle.
    // Determine which simplex we are in.
    var i1 = 0
    var j1 = 0 // Offsets for second (middle) corner of simplex in (i,j) coords
    if (x0 > y0) {
      i1 = 1
      j1 = 0
      // lower triangle, XY order: (0,0)->(1,0)->(1,1)}
    } else {
      i1 = 0
      j1 = 1
      // upper triangle, YX order: (0,0)->(0,1)->(1,1)}
      // A step of (1,0) in (i,j) means a step of (1-c,-c) in (x,y), and
      // a step of (0,1) in (i,j) means a step of (-c,1-c) in (x,y), where
      // c = (3-sqrt(3))/6
    }
    val x1 = x0 - i1 + G2 // Offsets for middle corner in (x,y) unskewed coords
    val y1 = y0 - j1 + G2
    val x2 = x0 - 1.0 + 2.0 * G2 // Offsets for last corner in (x,y) unskewed coords
    val y2 = y0 - 1.0 + 2.0 * G2
    // Work out the hashed gradient indices of the three simplex corners
    val ii = i & 255
    val jj = j & 255
    val gi0 = perm(ii + perm(jj)) % 12
    val gi1 = perm(ii + i1 + perm(jj + j1)) % 12
    val gi2 = perm(ii + 1 + perm(jj + 1)) % 12
    // Calculate the contribution from the three corners
    var t0 = 0.5 - x0 * x0 - y0 * y0
    if (t0 < 0) n0 = 0.0
    else {
      t0 *= t0
      n0 = t0 * t0 * dot(grad3(gi0), x0, y0) // (x,y) of grad3 used for 2D gradient

    }
    var t1 = 0.5 - x1 * x1 - y1 * y1
    if (t1 < 0) n1 = 0.0
    else {
      t1 *= t1
      n1 = t1 * t1 * dot(grad3(gi1), x1, y1)
    }
    var t2 = 0.5 - x2 * x2 - y2 * y2
    if (t2 < 0) n2 = 0.0
    else {
      t2 *= t2
      n2 = t2 * t2 * dot(grad3(gi2), x2, y2)
    }
    // Add contributions from each corner to values the final noise value.
    // The result is scaled to return values in the interval [-1,1].
    return 70.0 * (n0 + n1 + n2)
  }
}

class SimplexNoise(res: IntV2 = IntV2(512, 512), seed:Int = 0) extends Heightmap {

  override def heightAt(pos: V2): Scalar = SimplexNoise.noise(pos.x.toFloat, pos.y.toFloat)

}
