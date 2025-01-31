package utils.heightmap

import utils.math.*
import utils.math.planar.V2

import scala.util.Random
object PerlinNoise{
  private final val grad3 = Array(Array(1, 1, 0), Array(-1, 1, 0), Array(1, -1, 0), Array(-1, -1, 0), Array(1, 0, 1), Array(-1, 0, 1), Array(1, 0, -1), Array(-1, 0, -1), Array(0, 1, 1), Array(0, -1, 1), Array(0, 1, -1), Array(0, -1, -1))
  private final val p = Array(151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233, 7, 225, 140, 36, 103, 30, 69, 142, 8, 99, 37, 240, 21, 10, 23, 190, 6, 148, 247, 120, 234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117, 35, 11, 32, 57, 177, 33, 88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168, 68, 175, 74, 165, 71, 134, 139, 48, 27, 166, 77, 146, 158, 231, 83, 111, 229, 122, 60, 211, 133, 230, 220, 105, 92, 41, 55, 46, 245, 40, 244, 102, 143, 54, 65, 25, 63, 161, 1, 216, 80, 73, 209, 76, 132, 187, 208, 89, 18, 169, 200, 196, 135, 130, 116, 188, 159, 86, 164, 100, 109, 198, 173, 186, 3, 64, 52, 217, 226, 250, 124, 123, 5, 202, 38, 147, 118, 126, 255, 82, 85, 212, 207, 206, 59, 227, 47, 16, 58, 17, 182, 189, 28, 42, 223, 183, 170, 213, 119, 248, 152, 2, 44, 154, 163, 70, 221, 153, 101, 155, 167, 43, 172, 9, 129, 22, 39, 253, 19, 98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104, 218, 246, 97, 228, 251, 34, 242, 193, 238, 210, 144, 12, 191, 179, 162, 241, 81, 51, 145, 235, 249, 14, 239, 107, 49, 192, 214, 31, 181, 199, 106, 157, 184, 84, 204, 176, 115, 121, 50, 45, 127, 4, 150, 254, 138, 236, 205, 93, 222, 114, 67, 29, 24, 72, 243, 141, 128, 195, 78, 66, 215, 61, 156, 180)
  // To remove the need for index wrapping, double the permutation table length
  private final val perm:Array[Int] = p ++ p
  // precalc for perm(i) % 12
  private final val permMod12:Array[Int] = perm.map(_ % 12)


  // This method is a *lot* faster than using (int)Math.floor(x)
  @inline private def fastfloor(x: Float) = if (x > 0) x.toInt
  else x.toInt - 1

  @inline private def dot(g: Array[Int], x: Float, y: Float, z: Float) = g(0) * x + g(1) * y + g(2) * z

  @inline private def mix(a: Float, b: Float, t: Float) = (1 - t) * a + t * b

  @inline private def fade(t: Float) = t * t * t * (t * (t * 6 - 15) + 10)

  // Classic Perlin noise, 3D version
  def noise(xin: Float, yin: Float, zin: Float): Float = { // Find unit grid cell containing point
    var X = fastfloor(xin)
    var Y = fastfloor(yin)
    var Z = fastfloor(zin)
    // Get relative xyz coordinates of point within that cell
    val x = xin - X
    val y = yin - Y
    val z = zin - Z
    // Wrap the integer cells at 255 (smaller integer period can be introduced here)
    X = X & 255
    Y = Y & 255
    Z = Z & 255
    // Calculate a set of eight hashed gradient indices
    val gi000 = permMod12(X + perm(Y + perm(Z)))
    val gi001 = permMod12(X + perm(Y + perm(Z + 1)))
    val gi010 = permMod12(X + perm(Y + 1 + perm(Z)))
    val gi011 = permMod12(X + perm(Y + 1 + perm(Z + 1)))
    val gi100 = permMod12(X + 1 + perm(Y + perm(Z)))
    val gi101 = permMod12(X + 1 + perm(Y + perm(Z + 1)))
    val gi110 = permMod12(X + 1 + perm(Y + 1 + perm(Z)))
    val gi111 = permMod12(X + 1 + perm(Y + 1 + perm(Z + 1)))
    // The gradients of each corner are now:
    // g000 = grad3[gi000];
    // g001 = grad3[gi001];
    // g010 = grad3[gi010];
    // g011 = grad3[gi011];
    // g100 = grad3[gi100];
    // g101 = grad3[gi101];
    // g110 = grad3[gi110];
    // g111 = grad3[gi111];
    // Calculate noise contributions from each of the eight corners
    val n000 = dot(grad3(gi000), x, y, z)
    val n100 = dot(grad3(gi100), x - 1, y, z)
    val n010 = dot(grad3(gi010), x, y - 1, z)
    val n110 = dot(grad3(gi110), x - 1, y - 1, z)
    val n001 = dot(grad3(gi001), x, y, z - 1)
    val n101 = dot(grad3(gi101), x - 1, y, z - 1)
    val n011 = dot(grad3(gi011), x, y - 1, z - 1)
    val n111 = dot(grad3(gi111), x - 1, y - 1, z - 1)
    // Compute the fade curve value for each of x, y, z
    val u = fade(x)
    val v = fade(y)
    val w = fade(z)
    // Interpolate along x the contributions from each of the corners
    val nx00 = mix(n000, n100, u)
    val nx01 = mix(n001, n101, u)
    val nx10 = mix(n010, n110, u)
    val nx11 = mix(n011, n111, u)
    // Interpolate the four results along y
    val nxy0 = mix(nx00, nx10, v)
    val nxy1 = mix(nx01, nx11, v)
    // Interpolate the two last results along z
    val nxyz = mix(nxy0, nxy1, w)
    nxyz
  }
}

case class PerlinNoise(seed:Long = 0) extends Heightmap {

  val rand = new Random(seed).nextInt()
  override def heightAt(pos: V2): Scalar = PerlinNoise.noise(pos.x.toFloat , pos.y.toFloat, rand * 1.4f)

}
