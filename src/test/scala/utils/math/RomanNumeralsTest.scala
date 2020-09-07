package utils.math
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.AppendedClues._

class RomanNumeralsTest extends AnyFunSuite {

  test(""){
    for(i<- 1 to 3999) {
    println(i)
      val toRoman = RomanNumerals.toRoman(i)
      val fromRoman:Int = RomanNumerals.fromRoman(toRoman)
      assert(fromRoman == i) withClue(s"$i $toRoman $fromRoman")
    }
  }

}
