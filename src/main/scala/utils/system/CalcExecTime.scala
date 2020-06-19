package utils.system

object CalcExecTime {
  def apply(code: => Unit):StopwatchSample = {
    val s = new Stopwatch
    code
    s.sampleNow()
  }

  def withResult[T](code: => T):(T, StopwatchSample) = {
    val s = new Stopwatch
    val res:T = code
    (res, s.sampleNow())
  }

}
