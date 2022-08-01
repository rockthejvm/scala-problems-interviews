package com.rockthejvm.numbers

import scala.annotation.tailrec

object RecurringDecimals extends App {

  def fractionToRecurringDecimals(numerator: Int, denominator: Int): String = ???

  /*
    1/3 = 0.(3)
    1/2 = 0.5
    4/2 = 2
    1/6 = 0.1(6)
    1/333 = 0.(003) = 0.003003...
    1/7 = 0.(.....)
    1/2003 = 0.(very large set of recurring decimals)

    -1/2
    1/Int.MinValue


    -not 1/Int.MaxValue
   */

  println(fractionToRecurringDecimals(1, 3))
  println(fractionToRecurringDecimals(1, 2))
  println(fractionToRecurringDecimals(4, 2))
  println(fractionToRecurringDecimals(1, 6))
  println(fractionToRecurringDecimals(1, 333))
  println(fractionToRecurringDecimals(1, 7))
  println(fractionToRecurringDecimals(1, 2003))
  println(fractionToRecurringDecimals(-1, 2))
  println(fractionToRecurringDecimals(1, Int.MinValue))
}
