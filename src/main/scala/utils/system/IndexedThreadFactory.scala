package utils.system

import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicInteger

class IndexedThreadFactory(name:String) extends ThreadFactory{
  val i = new AtomicInteger(0)

  override def newThread(r: Runnable): Thread = {
    val id = i.getAndIncrement()
    new Thread(r, s"$name: $id")
  }
}
