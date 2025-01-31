package utils.math.space

import utils.math.planar.V2

case class Rectangle(

                      c: V3, // center point of rectangle
                      u: (V3, V3), // unit vectors determining local x and y axes for the rectangle
                      e: V2 // the halfwidth extents of the rectangle along the axes
                    ) extends ClosestPoint {
  def uId(i: Int): V3 = i match {
    case 0 => u._1
    case 1 => u._2
  }

  def fromRectCords(cords:V2):V3 = c + (u._1 * cords.x) + (u._2 * cords.y)

  def min :V3 = c + fromRectCords(-e)

  def max :V3 = c + fromRectCords(e)

  override def closestPoint(point: V3): V3 = {
    val d = point - c
    // Start result at center of rect; make steps from there
    var q = c
    // For each rect axis...
    var i:Int = 0
    while(i < 2){
      // ...project d onto that axis to get the distance
      // along the axis of d from the rect center
      var dist = d ** uId(i)
      // If distance farther than the rect extents, clamp to the rect
      if (dist > e(i)) dist = e(i)
      if (dist < -e(i)) dist = -e(i)
      // Step that distance along the axis to get world coordinate
      q += uId(i) * dist

      i += 1
    }
    return q
  }


}
