package com.rockthejvm.strings

import scala.annotation.tailrec

object CountCharacters {

  def countCharacters(s: String): Map[Char, Int] = {
    /*
      cct("Scala", []) = cct("cala", [S -> 1])
      = cct("ala", [c -> 1, S -> 1])
      = cct("la", [a -> 1, c -> 1, S -> 1])
      = cct("a", [l -> 1, a -> 1, c -> 1, S -> 1])
      = cct("", [a -> 2, l -> 1, c -> 1, S -> 1])
      = [a -> 2, l -> 1, c -> 1, S -> 1]
     */
    @tailrec
    def countCharactersTailrec(remaining: String, acc: Map[Char, Int]): Map[Char, Int] =
      if (remaining.isEmpty) acc
      else if (acc.contains(remaining.head)) {
        val currentChar = remaining.head
        val currentOccurrences = acc(currentChar)
        countCharactersTailrec(remaining.tail, acc + (currentChar -> (currentOccurrences + 1)))
      } else countCharactersTailrec(remaining.tail, acc + (remaining.head -> 1))

    countCharactersTailrec(s, Map())
  }

  def testCountCharacters() = {
    println(countCharacters("Scala"))
    println(countCharacters("I love Scala and functional programming because it's awesome!"))
  }

  def main(args: Array[String]): Unit = {
    testCountCharacters()
  }
}
