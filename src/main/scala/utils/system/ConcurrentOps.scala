package utils.system

import java.util.concurrent.locks.Lock
import scala.concurrent.blocking

object ConcurrentOps {
  def withLock[T](lock: Lock)(thunk: => T): T = {
    if (!lock.tryLock()) {
      blocking {
        lock.lock()
      }
    }
    try thunk finally lock.unlock()
  }

  def withoutLock[T](lock: Lock)(thunk: => T): T = {
    lock.unlock()
    try thunk finally lock.lock()
  }
}
