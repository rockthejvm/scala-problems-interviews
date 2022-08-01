package com.rockthejvm.strings

import scala.annotation.tailrec

object CountCharacters {

  def countCharacters(s: String): Map[Char, Int] = ???

  def testCountCharacters() = {
    println(countCharacters("Scala"))
    println(countCharacters("I love Scala and functional programming because it's awesome!"))
  }

  def main(args: Array[String]): Unit = {
    testCountCharacters()
  }
}
