package utils.math.planar

object AngleOps {
  //todo optimize if needed
  @inline def isCCW(v1:V2, v2:V2, v3:V2):Boolean = TrianglePlanar(v1, v2, v3).ccw

 /* @inline def leftBisector(v1:V2, v2:V2, v3:V2):Boolean = {
    val s1 = v1 - v2
    val s2 = v3 - v2
    val an
  }*/


}
