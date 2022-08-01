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
  def parseInteger(string: String): Int = ???

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
