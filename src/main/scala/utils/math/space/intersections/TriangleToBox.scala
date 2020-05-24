package utils.math.space.intersections

import utils.math.space.{Triangle, V3}
import utils.math._

/** ***************************************************************************/
/* AABB-triangle overlap test code                                            */
/* based on original c++ version by Tomas Akenine-Möller                      */
/*http://fileadmin.cs.lth.se/cs/Personal/Tomas_Akenine-Moller/code/tribox2.txt*/
////////////////////////////////////////////////////////////////////////////////
object TriangleToBox {


  def triangleToBoxIntersection(center: V3, extents: V3, triangle: Triangle): Boolean = {
    /*    use separating axis theorem to test overlap between triangle and box */
    /*    need to test for overlap in these directions: */
    /*    1) the {x,y,z}-directions (actually, since we use the AABB of the triangle */
    /*       we do not even need to test these) */
    /*    2) normal of the triangle */
    /*    3) crossproduct(edge from tri, {x,y,z}-directin) */
    /*       this gives 3x3=9 more tests */
    //float v0[3],v1[3],v2[3];
    //float min,max,d,p0,p1,p2,rad,fex,fey,fez;
    //float normal[3],e0[3],e1[3],e2[3];

    /* This is the fastest branch on Sun */
    /* move everything so that the boxcenter is in (0,0,0) */
    val v0 = triangle(0) - center
    val v1 = triangle(1) - center
    val v2 = triangle(2) - center

    /* compute triangle edges */
    val e0 = v1 - v0
    /* tri edge 0 */
    val e1 = v2 - v1
    /* tri edge 1 */
    val e2 = v0 - v2 /* tri edge 2 */

    /*======================== X-tests ========================*/
    @inline def AXISTEST_X01(a: Scalar, b: Scalar, fa: Scalar, fb: Scalar): Boolean = {
      val p0 = a * v0.y - b * v0.z
      val p2 = a * v2.y - b * v2.z
      var min:Scalar = 0
      var max:Scalar = 0
      if (p0 < p2) {
        min = p0
        max = p2
      } else {
        min = p2
        max = p0
      }
      val rad = fa * extents.y + fb * extents.z
      return min > rad || max < -rad
    }

    @inline def AXISTEST_X2(a: Scalar, b: Scalar, fa: Scalar, fb: Scalar): Boolean = {
      val p0 = a * v0.y - b * v0.z
      val p1 = a * v1.y - b * v1.z
      var min:Scalar = 0f
      var max:Scalar = 0f
      if (p0 < p1) {
        min = p0
        max = p1
      } else {
        min = p1
        max = p0
      }
      val rad = fa * extents.y + fb * extents.z
      return min > rad || max < -rad
    }

    /*======================== Y-tests ========================*/
    @inline def AXISTEST_Y02(a: Scalar, b: Scalar, fa: Scalar, fb: Scalar): Boolean = {

      val p0 = -a * v0.x + b * v0.z
      val p2 = -a * v2.x + b * v2.z
      var min:Scalar = 0f
      var max:Scalar = 0f
      if (p0 < p2) {
        min = p0
        max = p2
      } else {
        min = p2
        max = p0
      }
      val rad = fa * extents.x + fb * extents.z
      return min > rad || max < -rad
    }

    @inline def AXISTEST_Y1(a: Scalar, b: Scalar, fa: Scalar, fb: Scalar): Boolean = {
      val p0 = -a * v0.x + b * v0.z
      val p1 = -a * v1.x + b * v1.z
      var min:Scalar = 0f
      var max:Scalar = 0f
      if (p0 < p1) {
        min = p0
        max = p1
      } else {
        min = p1
        max = p0
      }
      val rad = fa * extents.x + fb * extents.z
      return min > rad || max < -rad
    }

    /*======================== Z-tests ========================*/
    @inline def AXISTEST_Z12(a: Scalar, b: Scalar, fa: Scalar, fb: Scalar): Boolean = {
      val p1 = a * v1.x - b * v1.y
      val p2 = a * v2.x - b * v2.y
      var min:Scalar = 0f
      var max:Scalar = 0f
      if (p2 < p1) {
        min = p2
        max = p1
      } else {
        min = p1
        max = p2
      }
      val rad = fa * extents.x + fb * extents.y
      return min > rad || max < -rad
    }

    @inline def AXISTEST_Z0(a: Scalar, b: Scalar, fa: Scalar, fb: Scalar): Boolean = {

      val p0 = a * v0.x - b * v0.y
      val p1 = a * v1.x - b * v1.y
      var min:Scalar = 0f
      var max:Scalar = 0f
      if (p0 < p1) {
        min = p0
        max = p1
      } else {
        min = p1
        max = p0
      }
      val rad = fa * extents.x + fb * extents.y
      return min > rad || max < -rad
    }

    /* Bullet 3:  */
    /*  test the 9 tests first (this was faster) */
    {
      val fex = math.abs(e0.x)
      val fey = math.abs(e0.y)
      val fez = math.abs(e0.z)
      if (AXISTEST_X01(e0.z, e0.y, fez, fey)) return false
      if (AXISTEST_Y02(e0.z, e0.x, fez, fex)) return false
      if (AXISTEST_Z12(e0.y, e0.x, fey, fex)) return false
    }
    {
      val fex = math.abs(e1.x)
      val fey = math.abs(e1.y)
      val fez = math.abs(e1.z)
      if (AXISTEST_X01(e1.z, e1.y, fez, fey)) return false
      if (AXISTEST_Y02(e1.z, e1.x, fez, fex)) return false
      if (AXISTEST_Z0(e1.y, e1.x, fey, fex)) return false
    }
    {
      val fex = math.abs(e2.x)
      val fey = math.abs(e2.y)
      val fez = math.abs(e2.z)
      if (AXISTEST_X2(e2.z, e2.y, fez, fey)) return false
      if (AXISTEST_Y1(e2.z, e2.x, fez, fex)) return false
      if (AXISTEST_Z12(e2.y, e2.x, fey, fex)) return false
    }
    /* Bullet 1: */
    /*  first test overlap in the {x,y,z}-directions */
    /*  find min, max of the triangle each direction, and test for overlap in */
    /*  that direction -- this is equivalent to testing a minimal AABB around */
    /*  the triangle against the AABB */

    /* test in X-direction */
    val minx = fmin(v0.x, v1.x, v2.x)
    val maxx = fmax(v0.x, v1.x, v2.x)
    if (minx > extents.x || maxx < -extents.x) return false

    /* test in Y-direction */
    val miny = fmin(v0.y, v1.y, v2.y)
    val maxy = fmax(v0.y, v1.y, v2.y)
    if (miny > extents.y || maxy < -extents.y) return false

    /* test in Z-direction */
    val minz = fmin(v0.z, v1.z, v2.z)
    val maxz = fmax(v0.z, v1.z, v2.z)
    if (minz > extents.z || maxz < -extents.z) return false

    /* Bullet 2: */
    /*  test if the box intersects the plane of the triangle */
    /*  compute plane equation of triangle: normal*x+d=0 */
    val normal = e0 ^ e1
    //val d = -(normal ** v0) /* plane eq: normal.x+d=0 */// -NJMP- (line removed here)
    if (!planeBoxOverlap(normal, v0, extents)) return false// -NJMP-

    return true; /* box and triangle overlaps */
  }

  @inline private def fmin(a: Scalar, b: Scalar, c: Scalar): Scalar = math.min(a, math.min(b, c))

  @inline private def fmax(a: Scalar, b: Scalar, c: Scalar): Scalar = math.max(a, math.max(b, c))

  def planeBoxOverlap(normal: V3, vert:V3, maxbox: V3): Boolean = {
    var i: Int = 0

    val min = V3(
      (if (normal.x > 0.0f) -maxbox.x else maxbox.x) - vert.x,
      (if (normal.y > 0.0f) -maxbox.y else maxbox.y) - vert.y,
      (if (normal.z > 0.0f) -maxbox.z else maxbox.z) - vert.z,
    )
    val max = V3(
      (if (normal.x > 0.0f) maxbox.x else -maxbox.x) - vert.x,
      (if (normal.y > 0.0f) maxbox.y else -maxbox.y) - vert.y,
      (if (normal.z > 0.0f) maxbox.z else -maxbox.z) - vert.z,
    )

    if (normal ** min  > 0.0f) return false
    if (normal ** max  >= 0.0f) return true

    return false
  }


}
