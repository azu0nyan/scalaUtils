package utils.datastructures.planar

import utils.datastructures.spatial.AARectangle

//TODO
sealed abstract class AABBTree[T](val aabb: AARectangle) {
  def add(ab: AARectangle, v: T): AABBTree[T]
}


private case class Leaf[T](override val aabb: AARectangle, data: T) extends AABBTree[T](aabb) {
  override def add(ab: AARectangle, newData: T): AABBTree[T] = Branch(this, Leaf(ab, newData))
}

private case class Branch[T](t1: AABBTree[T], t2: AABBTree[T]) extends AABBTree[T](t1.aabb combine t2.aabb) {
  override def add(ab: AARectangle, v: T): AABBTree[T] =
    if (ab.intersects(t1.aabb) && ab.intersects(t2.aabb)) {
      if (ab.intersection(t1.aabb).area > ab.intersection(t2.aabb).area) {
        Branch(t1.add(ab, v), t2)
      } else {
        Branch(t1, t2.add(ab, v))
      }
    } else if (ab.intersects(t1.aabb)) {
      Branch(t1.add(ab, v), t2)
    } else if (ab.intersects(t2.aabb)) {
      Branch(t1, t2.add(ab, v))
    } else if (t1.aabb.distanseTo(ab) > t2.aabb.distanseTo(ab)) {
      Branch(t1.add(ab, v), t2)
    } else {
      Branch(t1, t2.add(ab, v))
    }

}



