package utils.datastructures

import java.util.concurrent.atomic.AtomicInteger

class IdProvider {
  private val id:AtomicInteger = new AtomicInteger(0)

  def getNextId(): Int = id.getAndIncrement()
}
