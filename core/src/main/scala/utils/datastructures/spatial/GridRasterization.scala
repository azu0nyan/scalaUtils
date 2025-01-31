package utils.datastructures.spatial

import utils.math.*
import utils.math.planar.SegmentPlanar
import utils.math.space.Segment


// TODO make work

object GridRasterization {
  def rasterize2d(s: SegmentPlanar, cellSize: Scalar): Iterator[(Int, Int)] = rasterize2dAlgo(s.v1.x, s.v1.y, s.v2.x, s.v2.y, cellSize)

  def rasterize2dAlgo(x1: Scalar, y1: Scalar, x2: Scalar, y2: Scalar, cellSize: Scalar): Iterator[(Int, Int)] =
    new Iterator[(Int, Int)] {
      // Determine start grid cell coordinates (i, j)
      var i: Int = (x1 / cellSize).toInt
      var j: Int = (y1 / cellSize).toInt

      // Determine end grid cell coordinates (iend, jend)
      val iend: Int = (x2 / cellSize).toInt
      val jend: Int = (y2 / cellSize).toInt
      // Determine in which primary direction to step
      val di: Int = if (x1 < x2) 1 else (if (x1 > x2) -1 else 0)
      val dj: Int = if (y1 < y2) 1 else (if (y1 > y2) -1 else 0)

      // Determine tx and ty, the values of t at which the directed segment
      // (x1,y1)-(x2,y2) crosses the first horizontal and vertical cell
      // boundaries, respectively. Min(tx, ty) indicates how far one can
      // travel along the segment and still remain in the current cell
      val minx: Scalar = cellSize * floor(x1 / cellSize)
      val maxx: Scalar = minx + cellSize
      var tx: Scalar = (if (x1 > x2) (x1 - minx) else (maxx - x1)) / abs(x2 - x1)
      val miny: Scalar = cellSize * floor(y1 / cellSize)
      val maxy: Scalar = miny + cellSize
      var ty: Scalar = (if (y1 > y2) (y1 - miny) else (maxy - y1)) / abs(y2 - y1);
      // Determine deltax/deltay, how far (in units of t) one must step
      // along the directed line segment for the horizontal/vertical
      // movement (respectively) to equal the width/height of a cell
      val deltatx: Scalar = cellSize / abs(x2 - x1)
      val deltaty: Scalar = cellSize / abs(y2 - y1)

      var _hasNext: Boolean = true

      override def hasNext: Boolean = _hasNext

      /*Main loop. Visits cells until last cell reached
         while(true) {
           //visit (i, j);
           if (tx <= ty) { // tx smallest, step in x
             if (i == iend) break;
             tx += deltatx;
             i += di;
           } else { // ty smallest, step in y
             if (j == jend) break;
             ty += deltaty;
             j += dj;
           }
         }       */
      override def next(): (Int, Int) = {
        val res = (i, j)
        if (tx <= ty) { // tx smallest, step in x
          if (i == iend) {
            _hasNext = false
          }
          tx += deltatx;
          i += di;
        } else { // ty smallest, step in y
          if (j == jend) {
            _hasNext = false
          }
          ty += deltaty;
          j += dj;
        }
        res
      }
    }

  def rasterize3d(s: Segment, cellSize: Scalar): Iterator[(Int, Int, Int)] = rasterize3dAlgo(s.v1.x, s.v1.y, s.v1.z, s.v2.x, s.v2.y, s.v2.z, cellSize)

  def rasterize3dAlgo(x1: Scalar, y1: Scalar, z1: Scalar, x2: Scalar, y2: Scalar, z2: Scalar, cellSize: Scalar): Iterator[(Int, Int, Int)] =
    new Iterator[(Int, Int, Int)] {
      // look comments for 2d version
      var i: Int = (x1 / cellSize).toInt
      var j: Int = (y1 / cellSize).toInt
      var k: Int = (z1 / cellSize).toInt

      val iend: Int = (x2 / cellSize).toInt
      val jend: Int = (y2 / cellSize).toInt
      val kend: Int = (z2 / cellSize).toInt

      val di: Int = if (x1 < x2) 1 else (if (x1 > x2) -1 else 0)
      val dj: Int = if (y1 < y2) 1 else (if (y1 > y2) -1 else 0)
      val dk: Int = if (z1 < z2) 1 else (if (z1 > z2) -1 else 0)

      val minx: Scalar = cellSize * floor(x1 / cellSize)
      val maxx: Scalar = minx + cellSize
      var tx: Scalar = (if (x1 > x2) (x1 - minx) else (maxx - x1)) / abs(x2 - x1)
      val miny: Scalar = cellSize * floor(y1 / cellSize)
      val maxy: Scalar = miny + cellSize
      var ty: Scalar = (if (y1 > y2) (y1 - miny) else (maxy - y1)) / abs(y2 - y1);
      val minz: Scalar = cellSize * floor(z1 / cellSize)
      val maxz: Scalar = minz + cellSize
      var tz: Scalar = (if (z1 > z2) (z1 - minz) else (maxz - z1)) / abs(z2 - z1);

      val deltatx: Scalar = cellSize / abs(x2 - x1)
      val deltaty: Scalar = cellSize / abs(y2 - y1)
      val deltatz: Scalar = cellSize / abs(z2 - z1)

      override def hasNext: Boolean = _hasNext

      var _hasNext: Boolean = true

      override def next(): (Int, Int, Int) = {
        val res = (i, j, k)
        if (tx <= ty && tx <= tz) { // tx smallest, step in x
          if (i == iend) {
            _hasNext = false
          }
          tx += deltatx
          i += di
        } else if (ty <= tx && ty <= tz) { // ty smallest, step in y
          if (j == jend) {
            _hasNext = false
          }
          ty += deltaty
          j += dj
        } else { // tz smallest, step in z
          if (k == kend) {
            _hasNext = false
          }
          tz += deltatz
          k += dk
        }
        res
      }
    }

}
