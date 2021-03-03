package com.rockthejvm.numbers

import scala.annotation.tailrec

object ReverseInteger {

  // return a number with the digits reversed
  // if the result overflows Int, return 0
  def reverseInteger(number: Int): Int = {
    // assumes positive arguments
    @tailrec
    def reverseTailrec(remaining: Int, acc: Int): Int =
      if (remaining == 0) acc
      else {
        val digit = remaining % 10
        val tentativeResult = acc * 10 + digit

        // very careful
        if ((acc >= 0) != (tentativeResult >= 0)) 0
        else reverseTailrec(remaining / 10, tentativeResult)
      }

    // -2^31...2^31-1
    // Int.MinValue = 10000000000000000000000000000000
    // -Int.MinValue = 01111111111111111111111111111111 + 1 = 10000000000000000000000000000000 = Int.MinValue
    // -n = ~n + 1

    if (number == Int.MinValue) 0
    else if (number >= 0) reverseTailrec(number, 0)
    else -reverseTailrec(-number, 0)
  }

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
