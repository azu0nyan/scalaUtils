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

  def fromRoman(s:Seq[Char]):Int = (s:Seq[Char]) match {
    case 'I' :: 'V' :: r => 4 + fromRoman(r)
    case 'I' :: 'X' :: r => 9 + fromRoman(r)
    case 'I' :: r => 1 + fromRoman(r)
    case 'V' :: r => 5 + fromRoman(r)
    case 'X' :: 'L' :: r => 40 + fromRoman(r)
    case 'X' :: 'C' :: r => 90 + fromRoman(r)
    case 'X' :: r => 10 + fromRoman(r)
    case 'L' :: r => 50 + fromRoman(r)
    case 'C' :: 'M' :: r => 900 + fromRoman(r)
    case 'C' :: 'D' :: r => 400 + fromRoman(r)
    case 'C' :: r => 100 + fromRoman(r)
    case 'D' :: r => 500 + fromRoman(r)
    case 'M' :: r => 1000 + fromRoman(r)

  }

}
