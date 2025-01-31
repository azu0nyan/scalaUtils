package utils.system

import java.util.concurrent.CopyOnWriteArrayList
import java.util.logging.Logger
import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters.*

object Statistics {

  def times: TrieMap[String, CopyOnWriteArrayList[Long]] = new TrieMap[String, CopyOnWriteArrayList[Long]]()

  def logDeltaTimeNs(stat: String, timeNs: Long): Unit = {
    times.getOrElseUpdate(stat, new CopyOnWriteArrayList[Long]()).add(timeNs)
  }

  val log: Logger = Logger.getLogger("Statistics")

  def logStats(): Unit = {
    log.info("Statistics")
    log.info("Statistics delta times ns")
    times.foreach { kv =>
      val name = kv._1
      val total = kv._2.size()
      val avg: Double = kv._2.asScala.sum.toDouble / (if (total == 0) 1.0 else total.toDouble)
      log.info(f"$name%30s total: $total%10d avg: $avg%5.5f")
    }
  }

  def logDeltaTimeNs(stat: String)(code: => Unit): Long = {
    val start = System.nanoTime()
    code
    val end = System.nanoTime()
    val dt = end - start
    logDeltaTimeNs(stat, dt)
    dt
  }


}
