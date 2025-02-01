package utils.math.planar.algo.straightSkeleton

import utils.color.Color

/**
 * "Tags" for output properties of faces
 *
 */
class Tag(var name: String = "unnamed", var color: Color = Color(1, 0, 0), var colorName: String = "red") {
  
  override def toString = name + "(" + colorName + ")"
}

