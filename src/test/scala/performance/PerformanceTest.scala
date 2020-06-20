package performance


import java.util.concurrent.CountDownLatch

class PerformanceTest(toTest: () => Unit, name: String = "Def. name", announce: Boolean = true, count: Int = 1, threads: Int = 1) {
  var c = 0

  private def executeTests(): Unit = {
    var i: Int = 0
    while (i < count) {
      toTest.apply()
      i += 1
    }
  }


  def run(): Unit = {
    if (threads == 1) {
      val start = System.currentTimeMillis()
      executeTests()
      val end = System.currentTimeMillis()
      duration = (end - start) / 1000f
    } else {
      val l = new CountDownLatch(threads)
      val start = System.currentTimeMillis()
      for (i <- 0 until threads) {
        new Thread ( () => {
          executeTests()
          l.countDown()
        }).start()
      }
      l.await()
      val end = System.currentTimeMillis()
      duration = (end - start) / 1000f
    }

    if (announce) {
      println(f"$name: $duration%.3f")
    }
  }

  var duration: Double = -1

}


