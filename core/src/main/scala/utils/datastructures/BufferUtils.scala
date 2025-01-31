package utils.datastructures

import scala.collection.mutable

trait BufferUtils {
  import BufferUtils.*
  implicit def extendMutableBuffer[T](org: mutable.Buffer[T]): ExtendedBuffer[T] = new ExtendedBuffer(org)
}

object BufferUtils extends BufferUtils {

  implicit class ExtendedBuffer[T](val org: mutable.Buffer[T]) extends AnyVal {

    def beforeLast:T = org(org.size - 2)

    def removeIf(pred: (T) => Boolean): Unit = {
      // target holds the index we want to move the next element to
      var target = 0

      for (i <- org.indices;
           elem = org(i)
           if !pred(elem)) {

        org(target) = elem
        target += 1
      }

      org.remove(target, org.size - target)
    }
  }

}
