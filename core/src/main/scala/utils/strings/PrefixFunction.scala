package utils.strings

object PrefixFunction {
  def differentSubstrings(str: String): Int = {
    var k = 0
    var current = ""
    for (i <- 0 until str.length) {
      current = current + str.charAt(i)
      val current2 = new StringBuilder(current).reverse.toString
      val max = prefixFunction(current2.toCharArray)
      k += current.length - max
    }
    k
  }

  def prefixFunction(s: Array[Char]): Int = {
    var max = 0
    val prefix = new Array[Int](s.length)
    for (i <- 1 until prefix.length) {
      var k = prefix(i - 1)
      while ( {
        k > 0 && s(k) != s(i)
      }) k = prefix(k - 1) // length -> index
      if (s(k) == s(i)) {
        prefix(i) = k + 1
        max = Math.max(max, k + 1)
      }
      else prefix(i) = 0
    }
    max
  }
}
