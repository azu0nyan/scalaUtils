package utils.heightmap

import utils.math._
import utils.math.planar.V2

object GridOps {

  /**zero allocations, assuming grid and mix static*/
  def interpolateFromCellCenters[T](grid: (Int, Int) => T, resx: Int, resy: Int, mix: (T, T, Scalar) => T, posx: Scalar, posy: Scalar): T = {
    var cx: Int = posx.toInt
    var cy: Int = posy.toInt
    var inx: Scalar = posx % 1d
    var iny: Scalar = posy % 1d
    if (inx > 0.5d) {
      inx -= 0.5d
    } else {
      cx -= 1
      inx += 0.5d
    }
    if (iny > 0.5d) {
      iny -= 0.5d
    } else {
      cy -= 1
      iny += 0.5d
    }
    val lx: Int = clamp(cx, 0, resx - 1)
    val ly: Int = clamp(cy, 0, resy - 1)
    val rx: Int = clamp(cx + 1, 0, resx - 1)
    val ry: Int = clamp(cy + 1, 0, resy - 1)
    if (lx != rx && ly != ry) {
      val (v00, v01) = (grid(lx, ly), grid(rx, ly))
      val (v10, v11) = (grid(lx, ry), grid(rx, ry))
      mix(
        mix(v00, v01, inx),
        mix(v10, v11, inx),
        iny
      )
    } else if (lx != rx) {
      val (v00, v01) = (grid(lx, ly), grid(rx, ly))
      mix(v00, v01, inx)
    } else if (ly != ry) {
      val v00 = grid(lx, ly)
      val v10 = grid(lx, ry)
      mix(
        v00,
        v10,
        iny
      )
    } else {
      grid(lx, ly)
    }
  }

}
