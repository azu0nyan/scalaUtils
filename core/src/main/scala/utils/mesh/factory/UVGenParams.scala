package utils.mesh.factory

import utils.math.planar.V2

object UVGenParams{
  val defaultParams:UVGenParams = UVGenParams(V2.ZERO, V2(1d, 1d))
}

case class UVGenParams(
                        uvOffset: V2 = V2.ZERO,
                        uvScale: V2 = V2(1d, 1d )//edge length multiplier
                      ) {
  def transform(uv: V2):V2 =  uv * uvScale + uvOffset

}
