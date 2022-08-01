package com.rockthejvm.numbers

import scala.annotation.tailrec

object NumberOps {
  implicit class RRichInt(n: Int) { // our rich int
    /**
      * Easy problems
      */
    // check if a number is prime
    def isPrime: Boolean = ???

    // the constituent prime divisors
    def decompose: List[Int] = ???
  }

}

object NumberProblems extends App {

  import NumberOps._
  // implicit class RRichInt is available here


  def testIsPrime() = {
    println(2.isPrime) // new RRichInt(2).isPrime
    println(15.isPrime)
    println(2003.isPrime)
    println(2731189.isPrime)
    println(517935871.isPrime)
    println(1.isPrime)
    println(0.isPrime)
    println((-2003).isPrime)
  }



  def testDecompose() = {
    println(2.decompose)
    println(15.decompose)
    println(2003.decompose)
    println(2731189.decompose)
    println(517935871.decompose)
    println(1.decompose)
    println(0.decompose)
    println((-2003).decompose)
  }

  testDecompose()
}
