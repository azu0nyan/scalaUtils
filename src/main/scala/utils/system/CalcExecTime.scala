package utils.system

import utils.math.Scalar

object CalcExecTime {


  class CalcTimeStats(name: Option[String] = None,
                      private var warmupResetRun: Option[Int] = None) {

    var runs: Long = 0
    var timeTotalMillis: Long = 0
    var timeTotalNanos: Long = 0
    var maxMillis: Long = 0
    var minMillis: Long = Int.MaxValue
    var maxNanos:Long = 0
    var minNanos:Long = Int.MaxValue

    var lastSample: StopwatchSample = _

    def avgNanos: Scalar = timeTotalNanos / runs.toDouble
    def avgMillis: Scalar = timeTotalMillis / runs.toDouble
    def avgMsStr: String = f"$avgMillis%.2f ms"
    def avgNanosStr: String = f"$avgNanos%.0f us"

    def statsStr: String = s"${name.getOrElse("")} runs: $runs last: ${lastSample.msStr} avg: $avgMsStr"

    def longStatsStr:String = statsStr + f" min: $minMillis ms max: $maxMillis ms " +
      (if (avgMillis < 20) s"$avgNanosStr min $minNanos max $maxNanos" else " ")

    def apply[T](code: => T): T = {
      val s = new Stopwatch
      val res: T = code
      lastSample = s.sampleNow()
      if (warmupResetRun.contains(runs)) {
        runs = 0
        timeTotalMillis = 0
        timeTotalNanos = 0
        maxMillis = 0
        minMillis = Int.MaxValue
        maxNanos = 0
        minNanos = Int.MaxValue
        warmupResetRun = None
      }
      runs += 1
      timeTotalMillis += lastSample.dt.dt
      timeTotalNanos += lastSample.dNs
      maxMillis = math.max(lastSample.dt.dt, maxMillis)
      minMillis = math.min(lastSample.dt.dt, minMillis)
      maxNanos = math.max(lastSample.dNs, maxNanos)
      minNanos = math.min(lastSample.dNs, minNanos)
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
