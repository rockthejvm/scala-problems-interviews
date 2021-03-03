package com.rockthejvm.numbers

object ParseInteger {

  /*
    Return a number from the string argument:
    - there may be leading spaces, ignore those
    - read the sign character if present
    - read all the digits until the end of the string or until a non-digit character
    - return the number formed from those digits
    - if the number exceeds the int range, return either Int.MinValue (underflow) or Int.MaxValue (overflow)

    "   +1234 is the number I want" => 1234
   */
  def parseInteger(string: String): Int = {
    val WHITESPACE = ' '
    val PLUS = '+'
    val MINUS = '-'
    val DIGITS = "0123456789".toSet

    def integerRangeEnd(sign: Int): Int = if (sign >= 0) Int.MaxValue else Int. MinValue

    /*
      pt("123", -1, 0)
      = pt("23", -1, -1)
      = pt("3", -1, -12)
      = pt("", -1, -123)
      = -123
     */
    def parseTailrec(remainder: String, sign: Int, acc: Int = 0): Int =
      if (remainder.isEmpty || !DIGITS.contains(remainder.charAt(0))) acc
      else {
        val newDigit = remainder.charAt(0) - '0'
        val tentativeResult = acc * 10 + newDigit * sign

        if ((sign >= 0) != (tentativeResult >= 0)) integerRangeEnd(sign)
        else parseTailrec(remainder.substring(1), sign, tentativeResult)
      }

    if (string.isEmpty) 0
    else if (string.charAt(0) == WHITESPACE) parseInteger(string.substring(1))
    else if (string.charAt(0) == PLUS) parseTailrec(string.substring(1), sign = 1)
    else if (string.charAt(0) == MINUS) parseTailrec(string.substring(1), sign = -1)
    else parseTailrec(string, sign = 1)
  }

  def main(args: Array[String]): Unit = {
    println(parseInteger("")) // 0
    println(parseInteger("String")) // 0
    println(parseInteger("1")) // 1
    println(parseInteger("-1")) // -1
    println(parseInteger("   Scala")) // 0
    println(parseInteger("   4256")) // 4256
    println(parseInteger("   -4256")) // -4256
    println(parseInteger("   +4256")) // 4256
    println(parseInteger("42 is the meaning of life")) // 42
    println(parseInteger("  42 is the meaning of life")) // 42
    println(parseInteger(Int.MaxValue.toString)) // Int.MaxValue 2billion
    println(parseInteger(Int.MinValue.toString)) // Int.MinValue -2billion
    println(parseInteger("357893276583265783265783")) // Int.MaxValue
    println(parseInteger("-357893276583265783265783")) // Int.MinValue
  }
}
