package com.rockthejvm.numbers

import scala.annotation.tailrec

object ReverseInteger {

  // return a number with the digits reversed
  // if the result overflows Int, return 0
  def reverseInteger(number: Int): Int = ???

  def main(args: Array[String]): Unit = {
    // positives
    println("Positives:")
    println(reverseInteger(0))    // 0
    println(reverseInteger(9))    // 9
    println(reverseInteger(53))   // 35
    println(reverseInteger(504))  // 405
    println(reverseInteger(540))  // 45
    println(reverseInteger(53678534)) // 43587635
    println(reverseInteger(Int.MaxValue)) // 0
    // negatives
    println("Negatives:")
    println(reverseInteger(-9))     // -9
    println(reverseInteger(-53))    // -35
    println(reverseInteger(-504))   // -405
    println(reverseInteger(-540))   // -45
    println(reverseInteger(-53678534)) // -43587635
    println(reverseInteger(Int.MinValue)) // 0
  }
}
