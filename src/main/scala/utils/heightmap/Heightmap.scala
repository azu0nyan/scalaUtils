package utils.heightmap

import utils.datastructures.IntV2
import utils.math.planar.V2
import utils.math.space.V3
import utils.math._

object Heightmap {
  implicit def constantHeightmap(value: Scalar): Heightmap = (pos: V2) => value

  implicit def toV2ScalarMap(h: Heightmap): V2 => Scalar = h.heightAt

  implicit def toV2V3Map(h: Heightmap): V2 => V3 = h.worldPointAt

  def apply(f: V2 => Scalar): Heightmap = (pos: V2) => f(pos)

  def apply(h: Scalar): Heightmap = (_: V2) => h

}

trait Heightmap  {

  def apply(v: V2): Scalar = heightAt(v)

  def heightAt(pos: V2): Scalar

  def worldPointAt(pos: V2): V3 = pos.planarToV3(heightAt(pos))

  def *(other: Heightmap): Heightmap = (pos: V2) => this.heightAt(pos) * other.heightAt(pos)

  def +(other: Heightmap): Heightmap = (pos: V2) => this.heightAt(pos) + other.heightAt(pos)

  def /(other: Heightmap): Heightmap = (pos: V2) => if (other.heightAt(pos) != 0) this.heightAt(pos) / other.heightAt(pos) else 0

  def -(other: Heightmap): Heightmap = (pos: V2) => this.heightAt(pos) - other.heightAt(pos)

  def scaled(scale: Scalar): Heightmap = (pos: V2) => this.heightAt(pos / V2(scale, scale))

  def offsetOrigin(newOrigin: V2): Heightmap = (pos: V2) => this.heightAt(pos + newOrigin)

  def mapPositions(f: V2 => V2): Heightmap = (pos: V2) => this.heightAt(f apply pos)

  def map(f: Scalar => Scalar): Heightmap = (pos: V2) => f apply this.heightAt(pos)

  def lerp(start: Scalar, startVal: Scalar, end: Scalar, endVal: Scalar): Heightmap = map(utils.math.lerp(start, startVal, end, endVal, _))

  def toTileMap(res: IntV2): HeightGrid = HeightmapToHeightGrid(this, res)
}



