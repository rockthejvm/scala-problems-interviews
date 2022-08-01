package com.rockthejvm.numbers

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object UglyNumber {

  // ugly = only the factors 2, 3 and 5
  // 1 is ugly
  // assume positive inputs
  // examples: 6, 25, 100
  // not ugly: 14, 39
  @tailrec
  def uglyNumber(number: Int): Boolean = ???

  // the nth ugly number, given the index
  // 1 is the first ugly number
  def nthUgly(n: Int): Int = ???


  def main(args: Array[String]): Unit = {
    println(uglyNumber(1))
    println(uglyNumber(2))
    println(uglyNumber(3))
    println(uglyNumber(5))
    println(uglyNumber(1200)) // true
    println(uglyNumber(7 * 2 * 2 * 3 * 5 * 2 * 5 * 3)) // false
    // nth ugly
    println((1 to 100).map(nthUgly).toList)
  }
}
