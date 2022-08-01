package com.rockthejvm.numbers

import scala.annotation.tailrec

object Duplicates {

  // all numbers in the list appear EXACTLY twice, EXCEPT one: find that number
  def duplicates(list: List[Int]): Int = ???

  def main(args: Array[String]): Unit = {
    println(duplicates(List(1)))
    println(duplicates(List(1,2,1)))
    println(duplicates(List(1,2,3,2,1)))
    val first1000 = (1 to 100000).toList
    println(duplicates(first1000 ++ List(52369426) ++ first1000))
  }
}
