package utils.system

import utils.math.Scalar

object CalcExecTime {


  class CalcTimeStats(name:Option[String] = None,
                      private var warmupResetRun:Option[Int] = None) {

    var runs: Long = 0
    var timeTotalMs: Long = 0
    var timeTotalNS: Long = 0

    var lastSample: StopwatchSample = _

    def avgMs: Scalar = timeTotalMs / runs.toDouble

    def avgMsStr: String = f"$avgMs%.2f"

    def statsStr: String = s"${name.getOrElse("")} runs: $runs last: ${lastSample.msStr} avg: $avgMsStr"

    def apply[T](code: => T):T = {
      val s = new Stopwatch
      val res: T = code
      lastSample = s.sampleNow()
      if(warmupResetRun.contains(runs)){
        runs = 0
        timeTotalMs = 0
        timeTotalNS = 0
        warmupResetRun = None
      }
      runs += 1
      timeTotalMs += lastSample.dt.dt
      timeTotalNS += lastSample.dNs
      res
    }

  }

  def apply(code: => Unit): StopwatchSample = {
    val s = new Stopwatch
    code
    s.sampleNow()
  }

  def withResult[T](code: => T): (T, StopwatchSample) = {
    val s = new Stopwatch
    val res: T = code
    (res, s.sampleNow())
  }

}
