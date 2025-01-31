package utils.strings

object ManachersAlgo {

  def manachersAlgoCount(toTest: String): Long =
    manacherOddCount(toTest) + manacherEvenCount(toTest) +
      toTest.length


  def manacherOddCount(s: String): Long = {
    var sum = 0L
    var l = 0L
    var r = -1L
    val d = new Array[Long](s.length)
    for (i <- 0 until s.length) {
      var size = 0L
      if (i > r) size = 0
      else {
        val j = (r - i) + l
        size = Math.min((r - i).toLong, d(j.toInt))
      }
      while ( {
        i - size - 1 >= 0 && i + size + 1 < s.length && s.charAt((i - size - 1).toInt) ==
          s.charAt((i + size + 1).toInt)
      }) size += 1
      d(i) = size
      sum += d(i)
      if (i + size > r) {
        r = i + size
        l = i - size
      }
    }
    sum
  }

  def manacherEvenCount(s: String): Long = {
    var sum = 0L
    var l = 0L
    var r = -1L
    val d = new Array[Long](s.length)
    for (i <- 0 until s.length) {
      var size = 0L
      if (i > r) size = 0
      else {
        val j = (r - i) + l + 1
        size = Math.min((r - i) + 1, d(j.toInt))
      }
      while ( {
        i - size - 1 >= 0 && i + size < s.length && s.charAt((i - size - 1).toInt) == s.charAt((i + size).toInt)
      }) size += 1
      d(i) = size
      sum += d(i)
//      println(s"$i ${d(i)} $sum")
      if (i + size - 1 > r) {
        l = i - size
        r = i + size - 1
      }
    }
//    println(d.max)
    sum
  }

}
