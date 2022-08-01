package com.rockthejvm.numbers

object LargestNumber {

  /*
      Given a list of non-negative integers, arrange them such that they form the largest number.
      The result might be huge so return a string.

      List(10, 2) => "210"
      List(3, 30, 5, 9, 34) => "9534330"
   */
  def largestNumber(numbers: List[Int]): String = ???

  def main(args: Array[String]): Unit = {
    println(largestNumber(List(10, 2))) // 210
    println(largestNumber(List(3, 30, 5, 9, 34))) // 9534330
    println(largestNumber(List(2020, 20, 1010, 10, 2, 22))) // 222202020101010
    println(largestNumber(List(1))) // 1
    println(largestNumber(List())) // 0
    println(largestNumber(List(0, 0, 0))) // 0
  }

}
