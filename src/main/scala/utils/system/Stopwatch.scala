package utils.system

import utils.system.SClock.Timestamp

case class StopwatchSample(dt:Timestamp, dNs:Long) {
  def msStr:String = s"${dt.dt} ms."

}

class Stopwatch {
  val start:Timestamp = SClock.now()
  val startNs:Long = System.nanoTime()

  def sampleNow():StopwatchSample = StopwatchSample(start.now(), System.nanoTime() - startNs)
}


