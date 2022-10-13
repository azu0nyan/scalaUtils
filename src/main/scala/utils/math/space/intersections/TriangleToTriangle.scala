package utils.math.space.intersections

import utils.math.space.V3
import utils.math._

object TriangleToTriangle {
  /** based on c triangle/triangle intersection test routine,
    * by Tomas Moller, 1997.
    * http://fileadmin.cs.lth.se/cs/Personal/Tomas_Akenine-Moller/code/opttritri.txt
    * parameters: vertices of triangle 1: V0,V1,V2
    * vertices of triangle 2: U0,U1,U2
    */
  def apply(V0: V3, V1: V3, V2: V3, U0: V3, U1: V3, U2: V3): Boolean = {

    /* compute plane equation of triangle(V0,V1,V2) */
    var E1 = V1 - V0
    var E2 = V2 - V0
    val N1 = E1 ^ E2
    val d1 = -N1 ** V0
    /* plane equation 1: N1.X+d1=0 */

    /* put U0,U1,U2 into plane equation 1 to compute signed distances to the plane*/
    val du0 = N1 ** U0 + d1
    val du1 = N1 ** U1 + d1
    val du2 = N1 ** U2 + d1

    /* coplanarity robustness check */
    /*#if USE_EPSILON_TEST==TRUE
    if(FABS(du0)<EPSILON) du0=0.0;
    if(FABS(du1)<EPSILON) du1=0.0;
    if(FABS(du2)<EPSILON) du2=0.0;
    #endif*/

    var du0du1 = du0 * du1
    var du0du2 = du0 * du2

    if (du0du1 > 0.0f && du0du2 > 0.0f) return false /* same sign on all of them + not equal 0 ? */
    /* no intersection occurs */

    /* compute plane of triangle (U0,U1,U2) */
    E1 = U1 - U0
    E2 = U2 - U0
    val N2 = E1 ^ E2
    val d2 = -(N2 ** U0)
    /* plane equation 2: N2.X+d2=0 */

    /* put V0,V1,V2 into plane equation 2 */
    val dv0 = N2 ** V0 + d2
    val dv1 = N2 ** V1 + d2
    val dv2 = N2 ** V2 + d2

    /*#if USE_EPSILON_TEST==TRUE
    if(FABS(dv0)<EPSILON) dv0=0.0;
    if(FABS(dv1)<EPSILON) dv1=0.0;
    if(FABS(dv2)<EPSILON) dv2=0.0;
    #endif
     */
    val dv0dv1 = dv0 * dv1
    val dv0dv2 = dv0 * dv2

    //todo check fix
    if (dv0dv1 > 0.0f && dv0dv2 > 0.0f) return false /* same sign on all of them + not equal 0 ? *//* no intersection occurs */


    /* compute direction of intersection line */
    val D = N1 ^ N2

    /* compute and index to the largest component of D */
    var max = math.abs(D(0))
    var index = 0
    val bb = math.abs(D(1))
    val cc = math.abs(D(2))
    if (bb > max) {
      max = bb
      index = 1
    }
    if (cc > max) {
      max = cc
      index = 2
    }
    /* this is the simplified projection onto L*/
    val vp0 = V0(index)
    val vp1 = V1(index)
    val vp2 = V2(index)

    val up0 = U0(index)
    val up1 = U1(index)
    val up2 = U2(index)

    val nc1: (Scalar, Scalar, Scalar, Scalar, Scalar) = NEWCOMPUTE_INTERVALS(vp0, vp1, vp2, dv0, dv1, dv2, dv0dv1, dv0dv2) match {
      case Some(x) => x
      case None => return coplanar_tri_tri(N1, V0, V1, V2, U0, U1, U2)
    }

    @inline def a: Scalar = nc1._1

    @inline def b: Scalar = nc1._2

    @inline def c: Scalar = nc1._3

    @inline def x0: Scalar = nc1._4

    @inline def x1: Scalar = nc1._5

    val nc2: (Scalar, Scalar, Scalar, Scalar, Scalar) = NEWCOMPUTE_INTERVALS(up0, up1, up2, du0, du1, du2, du0du1, du0du2) match {
      case Some(x) => x
      case None => return coplanar_tri_tri(N1, V0, V1, V2, U0, U1, U2)
    }

    @inline def d: Scalar = nc2._1

    @inline def e: Scalar = nc2._2

    @inline def f: Scalar = nc2._3

    @inline def y0: Scalar = nc2._4

    @inline def y1: Scalar = nc2._5

    val xx = x0 * x1
    val yy = y0 * y1
    val xxyy = xx * yy

    var tmp = a * xxyy
    var isect1 = (tmp + b * x1 * yy, tmp + c * x0 * yy)

    if (isect1._1 > isect1._2) isect1 = isect1.swap

    tmp = d * xxyy
    var isect2 = (tmp + e * xx * y1, tmp + f * xx * y0)
    if (isect2._1 > isect2._2) isect2 = isect2.swap


    if (isect1._2 < isect2._1 || isect2._2 < isect1._1) return false
    return true
  }

  @inline def NEWCOMPUTE_INTERVALS(VV0: Scalar, VV1: Scalar, VV2: Scalar, D0: Scalar, D1: Scalar, D2: Scalar, D0D1: Scalar, D0D2: Scalar): Option[(Scalar, Scalar, Scalar, Scalar, Scalar)] = {
    if (D0D1 > 0.0f) {
      /* here we know that D0D2<=0.0 */
      /* that is D0, D1 are on the same side, D2 on the other or on the plane */
      return Some((VV2, (VV0 - VV2) * D2, (VV1 - VV2) * D2, D2 - D0, D2 - D1))
    }
    else if (D0D2 > 0.0f) {
      /* here we know that d0d1<=0.0 */
      return Some((VV1, (VV0 - VV1) * D1, (VV2 - VV1) * D1, D1 - D0, D1 - D2))
    }
    else if (D1 * D2 > 0.0f || D0 != 0.0f) {
      /* here we know that d0d1<=0.0 or that D0!=0.0 */
      return Some((VV0, (VV1 - VV0) * D0, (VV2 - VV0) * D0, D0 - D1, D0 - D2))
    }
    else if (D1 != 0.0f) {
      return Some((VV1, (VV0 - VV1) * D1, (VV2 - VV1) * D1, D1 - D0, D1 - D2))
    }
    else if (D2 != 0.0f) {
      return Some((VV2, (VV0 - VV2) * D2, (VV1 - VV2) * D2, D2 - D0, D2 - D1))
    }
    else {
      /* triangles are coplanar */
      return None
    }
  }

  def coplanar_tri_tri(N: V3, V0: V3, V1: V3, V2: V3, U0: V3, U1: V3, U2: V3): Boolean = {

    var i0 = 0
    var i1 = 0
    /* first project onto an axis-aligned plane, that maximizes the area */
    /* of the triangles, compute indices: i0,i1. */
    val A: Array[Scalar] = Array(math.abs(N(0)), math.abs(N(1)), math.abs(N(2)))
    if (A(0) > A(1)) {
      if (A(0) > A(2)) {
        i0 = 1; /* A(0) is greatest */
        i1 = 2
      }
      else {
        i0 = 0; /* A(2) is greatest */
        i1 = 1
      }
    } else /* A(0)<=A(1) */ {
      if (A(2) > A(1)) {
        i0 = 0; /* A(2) is greatest */
        i1 = 1
      }
      else {
        i0 = 0; /* A(1) is greatest */
        i1 = 2
      }
    }

    def EDGE_AGAINST_TRI_EDGES(V0: V3, V1: V3, U0: V3, U1: V3, U2: V3): Boolean = {
      //float Ax,Ay,Bx,By,Cx,Cy,e,d,f;
      val Ax = V1(i0) - V0(i0)
      val Ay = V1(i1) - V0(i1)

      /* this edge to edge test is based on Franlin Antonio's gem:
   "Faster Line Segment Intersection", in Graphics Gems III,
   pp. 199-202 */
      def EDGE_EDGE_TEST(V0: V3, U0: V3, U1: V3): Boolean = {
        val Bx = U0(i0) - U1(i0)

        val By = U0(i1) - U1(i1)

        val Cx = V0(i0) - U0(i0)

        val Cy = V0(i1) - U0(i1)

        val f = Ay * Bx - Ax * By

        val d = By * Cx - Bx * Cy

        if ((f > 0 && d >= 0 && d <= f) || (f < 0 && d <= 0 && d >= f)) {

          val e = Ax * Cy - Ay * Cx

          if (f > 0) {
            return e >= 0 && e <= f
          } else {
            return e <= 0 && e >= f
          }
        } else {
          return false
        }
      }

      /* test edge U0,U1 against V0,V1 */
      if (EDGE_EDGE_TEST(V0, U0, U1)) return true //else no return
      /* test edge U1,U2 against V0,V1 */
      if (EDGE_EDGE_TEST(V0, U1, U2)) return true //else no return
      /* test edge U2,U1 against V0,V1 */
      if (EDGE_EDGE_TEST(V0, U2, U0)) return true //else no return

      return false
    }

    /* test all edges of triangle 1 against the edges of triangle 2 */
    if (EDGE_AGAINST_TRI_EDGES(V0, V1, U0, U1, U2)) return true // else no return
    if (EDGE_AGAINST_TRI_EDGES(V1, V2, U0, U1, U2)) return true // else no return
    if (EDGE_AGAINST_TRI_EDGES(V2, V0, U0, U1, U2)) return true // else no return


    def POINT_IN_TRI(V0: V3, U0: V3, U1: V3, U2: V3): Boolean = {
      //  float a,b,c,d0,d1,d2                     
      /* is T1 completly inside T2? */
      /* check if V0 is inside tri(U0,U1,U2) */
      var a = U1(i1) - U0(i1)
      var b = -(U1(i0) - U0(i0))
      var c = -a * U0(i0) - b * U0(i1)
      val d0 = a * V0(i0) + b * V0(i1) + c

      a = U2(i1) - U1(i1)
      b = -(U2(i0) - U1(i0))
      c = -a * U1(i0) - b * U1(i1)
      val d1 = a * V0(i0) + b * V0(i1) + c

      a = U0(i1) - U2(i1)
      b = -(U0(i0) - U2(i0))
      c = -a * U2(i0) - b * U2(i1)
      val d2 = a * V0(i0) + b * V0(i1) + c
      if (d0 * d1 > 0.0) {
        return d0 * d2 > 0.0
      } else {
        return false
      }
    }

    /* finally, test if tri1 is totally contained in tri2 or vice versa */
    if (POINT_IN_TRI(V0, U0, U1, U2)) return true // else no return
    if (POINT_IN_TRI(U0, V0, V1, V2)) return true // else no return
    return false
  }


}


