package utils.system

import utils.system.SClock.Timestamp

case class StopwatchSample(dt:Timestamp, dNs:Long)

class Stopwatch {
  val start:Timestamp = SClock.now()
  val startNs:Long = System.nanoTime()

  def sampleNow():StopwatchSample = StopwatchSample(start.now(), System.nanoTime() - startNs)
}


