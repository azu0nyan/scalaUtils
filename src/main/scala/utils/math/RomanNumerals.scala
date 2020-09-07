package utils.math

object RomanNumerals {

  val num: Array[Int] = Array(1, 4, 5, 9, 10, 40, 50, 90, 100, 400, 500, 900, 1000).reverse
  val sym: Array[String] = Array("I", "IV", "V", "IX", "X", "XL", "L", "XC", "C", "CD", "D", "CM", "M").reverse


  def toRoman(x:Int):String = {
    val sb = new StringBuilder
    var n:Int  = x
    var i = num.length - 1
    while (n > 0){
      val div = n / num(i)
       n = n % num(i)
      sb.append(sym(i).repeat(div))
      i -= 1
    }
    sb.toString()
  }


  def fromRoman(s:String):Int = s.length match {
    case 0 => 0
    case 1 => map(s)
    case _ => map.get(s.substring(0, 2)) match {
      case Some(x) => x + fromRoman(s.substring(2))
      case None =>map(s.substring(0, 1)) + fromRoman(s.substring(1))
    }
  }


  val map:Map[String, Int] = (for(i<- num.indices) yield (sym(i) -> num(i))).toMap

}
