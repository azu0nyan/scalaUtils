package utils.strings

object ManachersAlgo {

  def manachersAlgoCount(toTest: String): Long =
    manacherOddCount(toTest) + manacherEvenCount(toTest) +
      toTest.length


  def manacherOddCount(s: String): Long = {
    var sum = 0
    var l = 0
    var r = -1
    val d = new Array[Int](s.length)
    for (i <- 0 until s.length) {
      var size = 0
      if (i > r) size = 0
      else {
        val j = (r - i) + l
        size = Math.min(r - i, d(j))
      }
      while ( {
        i - size - 1 >= 0 && i + size + 1 < s.length && s.charAt(i - size - 1) == s.charAt(i + size + 1)
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
    var sum = 0
    var l = 0
    var r = -1
    val d = new Array[Int](s.length)
    for (i <- 0 until s.length) {
      var size = 0
      if (i > r) size = 0
      else {
        val j = (r - i) + l + 1
        size = Math.min((r - i) + 1, d(j))
      }
      while ( {
        i - size - 1 >= 0 && i + size < s.length && s.charAt(i - size - 1) == s.charAt(i + size)
      }) size += 1
      d(i) = size
      sum += d(i)
      if (i + size - 1 > r) {
        l = i - size
        r = i + size - 1
      }
    }
    sum
  }

}
