package com.rockthejvm.numbers

import scala.annotation.tailrec

object NumberOps {
  implicit class RRichInt(n: Int) { // our rich int
    /**
      * Easy problems
      */
    // check if a number is prime
    def isPrime: Boolean = {
      /*
        isPrime(11) = ipt(2)
        = 11 % 2 != 0 && ipt(3)
        = ipt(3)
        = 11 % 3 != 0 && ipt(4)
        = ipt(4)
        = true

        isPrime(15) = ipt(2)
        = 15 % 2 != 0 && ipt(3) = ipt(3)
        = 15 % 3 != 0 && ipt(4)
        = false

        Complexity: O(sqrt(N))
       */
      @tailrec
      def isPrimeTailrec(currentDivisor: Int): Boolean = {
        if (currentDivisor > Math.sqrt(Math.abs(n))) true
        else n % currentDivisor != 0 && isPrimeTailrec(currentDivisor + 1)
      }

      if (n == 0 || n == 1 || n == -1) false
      else isPrimeTailrec(2)
    }

    // the constituent prime divisors
    def decompose: List[Int] = {
      assert(n >= 0)

      /*
        decompose(11) = decomposeTailrec(11, 2, [])
        = decomposeTailrec(11, 3, [])
        = decomposeTailrec(11, 4, [])
        = [11]

        decompose(15) = decomposeTailrec(15, 2, [])
        = decomposeTailrec(15, 3, [])
        = decomposeTailrec(5, 3, [3])
        = [5,3]

        decompose(16) = decomposeTailrec(16, 2, [])
        = decomposeTailrec(8, 2, [2])
        = decomposeTailrec(4, 2, [2,2])
        = decomposeTailrec(2, 2, [2,2,2])
        = [2,2,2,2]

        Complexity: O(sqrt(N)); can be as low as O(log(N))
       */
      @tailrec
      def decomposeTailrec(remaining: Int, currentDivisor: Int, accumulator: List[Int]): List[Int] = {
        if (currentDivisor > Math.sqrt(remaining)) remaining :: accumulator
        else if (remaining % currentDivisor == 0) decomposeTailrec(remaining / currentDivisor, currentDivisor, currentDivisor :: accumulator)
        else decomposeTailrec(remaining, currentDivisor + 1, accumulator)
      }

      decomposeTailrec(n, 2, List())
    }
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
