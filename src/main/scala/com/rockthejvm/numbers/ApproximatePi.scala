package com.rockthejvm.numbers

import scala.util.Random

object ApproximatePi extends App {

  val random = new Random(System.currentTimeMillis())

  // compute Pi using Monte-Carlo
  def approximatePi(nPoints: Int): Double = ???

  println(s"Reference: ${Math.PI}")
  println(approximatePi(1000))
  println(approximatePi(10000))
  println(approximatePi(100000))
  println(approximatePi(1000000))
  println(approximatePi(10000000))
  println(approximatePi(100000000))

}
