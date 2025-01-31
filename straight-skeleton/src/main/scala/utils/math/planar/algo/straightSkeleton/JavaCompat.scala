package utils.math.planar.algo.straightSkeleton

import utils.math.space.V3

object JavaCompat {

  @inline def normalizeJava(v: V3): V3 = {
    val norm = 1.0 / Math.sqrt(v.x * v.x + v.y * v.y + v.z * v.z)
    v * norm
  }
}
