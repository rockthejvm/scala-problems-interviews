package com.rockthejvm.numbers

object LargestNumber {

  /*
      Given a list of non-negative integers, arrange them such that they form the largest number.
      The result might be huge so return a string.

      List(10, 2) => "210"
      List(3, 30, 5, 9, 34) => "9534330"
   */
  def largestNumber(numbers: List[Int]): String = {
    // "given" instance in Scala 3
    implicit val newOrdering: Ordering[Int] = Ordering.fromLessThan { (a, b) =>
      // concatenate a with b => ab
      // concatenate b with a => ba
      // comparison: a comes before b if ab >= ba
      val aString = a.toString
      val bString = b.toString

      (aString + bString).compareTo(bString + aString) >= 0
    }
    /*
      - reflexive: a <= a
      - anti-symmetrical: if a <= b AND b <= a then a == b
        THIS IS NOT THE CASE for proper sorting
        List(1010, 10) counterexample
        for our problem IT DOES NOT MATTER
      - transitive: if a <= b AND b <= c then a <= c
     */

    val largest = numbers.sorted.mkString("")

    if (numbers.isEmpty || largest.charAt(0) == '0') "0" // List(0, 0, 0) => "000"
    else largest
  }

  def main(args: Array[String]): Unit = {
    println(largestNumber(List(10, 2))) // 210
    println(largestNumber(List(3, 30, 5, 9, 34))) // 9534330
    println(largestNumber(List(2020, 20, 1010, 10, 2, 22))) // 222202020101010
    println(largestNumber(List(1))) // 1
    println(largestNumber(List())) // 0
    println(largestNumber(List(0, 0, 0))) // 0
  }

}
