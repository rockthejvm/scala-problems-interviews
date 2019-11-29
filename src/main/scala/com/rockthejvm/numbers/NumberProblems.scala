package com.rockthejvm.numbers

import scala.annotation.tailrec

object NumberProblems extends App {

  def isPrime(n: Int): Boolean = {
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

  println(isPrime(2))
  println(isPrime(15))
  println(isPrime(2003))
  println(isPrime(2731189))
  println(isPrime(517935871))
  println(isPrime(1))
  println(isPrime(0))
  println(isPrime(-2003))
}
