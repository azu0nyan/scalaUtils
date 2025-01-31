package utils.math
import org.scalatest.AppendedClues.*
import org.scalatest.funsuite.AnyFunSuite

class RomanNumeralsTest extends AnyFunSuite {

  test("all to from"){
    for(i<- 1 to 3999) {
    println(i)
      val toRoman = RomanNumerals.toRoman(i)
      val fromRoman:Int = RomanNumerals.fromRoman(toRoman)
      assert(fromRoman == i) withClue(s"$i $toRoman $fromRoman")
    }
  }

  test("some"){
    assert(RomanNumerals.toRoman(1) == "I")
    assert(RomanNumerals.toRoman(4) == "IV")
    assert(RomanNumerals.toRoman(99) == "XCIX")
    assert(RomanNumerals.toRoman(101) == "CI")
    assert(RomanNumerals.toRoman(1001) == "MI")

  }

}
