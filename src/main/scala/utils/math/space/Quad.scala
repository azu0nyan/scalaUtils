package utils.math.space

import utils.math.planar.{QuadPlanar, V2}

/*
vertices
2 3
0 1
 */


case class Quad(val bl: V3, val br: V3, val tl: V3, val tr: V3) {
  def leftSideBT: V3 = tl - bl

  def rightSideBT: V3 = tr - br

  def topSideLR: V3 = tr - tl

  def botSideLR: V3 = br - bl

  def normal: UnitV3 = leftSideBT ^ botSideLR

  def flipVertical: Quad = new Quad(tl, tr, bl, br)

  def flipHorizontal: Quad = new Quad(br, bl, tr, tl)

  def toPlanarQuad2: QuadPlanar = {
//    val u : V3 = botSideLR.opposite.normalize
//    val v : V3 =  (u ^ normal).opposite.normalize
    val u : V3 = botSideLR.normalize
    val v : V3 =  (u ^ normal).normalize

    val bl2:V2 = bl.toBasis(u,v, normal).dropZ
    val br2:V2 = br.toBasis(u,v, normal).dropZ
    val tl2:V2 = tl.toBasis(u,v, normal).dropZ
    val tr2:V2 = tr.toBasis(u,v, normal).dropZ

   new QuadPlanar(bl2, br2, tl2, tr2)
   //new Quad2(V2(0, 0), V2(0, 1), V2(1, 0), V2(1, 1))
  }
}
