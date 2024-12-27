package utils.math.planar.algo.straightSkeleton

import java.awt.Color
import java.util.Comparator
import java.util

/**
 * "Tags" for output properties of faces
 *
 */
object Tag {
  var nameComparator = new Comparator[Tag]() {
    override def compare(o1: Tag, o2: Tag) = String.CASE_INSENSITIVE_ORDER.compare(o1.name, o2.name)
  }
}
class Tag(var name: String = "unnamed", var color: Color = Color.RED, var colorName: String = "red") {
  
  override def toString = name + "(" + colorName + ")"
}

