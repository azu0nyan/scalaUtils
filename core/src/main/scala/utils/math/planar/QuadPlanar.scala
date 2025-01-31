package utils.math.planar

import utils.math.*
import utils.math.space.Quad

class QuadPlanar(val bl: V2, val br: V2, val tl: V2, val tr: V2)
  extends PolygonRegion(
    Seq(bl, tl, tr, br)
  ) {

  def toQuad3(height: Scalar): Quad = new Quad(bl.planarToV3(height), br.planarToV3(height), tl.planarToV3(height), tr.planarToV3(height))

}

