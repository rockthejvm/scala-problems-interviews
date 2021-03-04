package com.rockthejvm.strings

import scala.collection.immutable.TreeMap

object ReorganizeString {

  // "aaabc" -> "abaca", "acaba"
  // "aaa" -> ""
  // rearrange chars so that no two adjacent chars are identical
  def reorganizeString(string: String): String = {

    /*
      {a-3, b-1, c-1}
      ot({a-3, b-1, c-1}, \0 "") =
      ot({a-2, b-1, c-1}, 'a', "a") =
      ot({a-2, c-1}, 'b', "ab") =
      ot({a-1, c-1}, 'a', "aba") =
      ot({a-1}, 'c', "abac") =
      ot({}, 'a', "abaca") =
      "abaca"

      Complexity: O(N^2) time
      Better complexity: use TreeMap: O(N*log(N))
     */
    def organizeTailrec(characterCount: Map[Char, Int], forbiddenChar: Char = '\u0000', acc: String = ""): String =
      if (characterCount.isEmpty) acc
      else {
        val newChar = characterCount.filter(_._1 != forbiddenChar).maxBy(_._2)._1
        val newCharCount =
          if (characterCount(newChar) == 1) characterCount - newChar
          else characterCount + (newChar -> (characterCount(newChar) - 1)) // log(N) time on a treemap

        organizeTailrec(newCharCount, newChar, acc + newChar)
      }

    implicit val tupleOrdering = Ordering.by[(Char, Int), Int](_._2)


    val charCountQuicker: Map[Char, Int] = string.groupBy(c => c).view.mapValues(_.length).toMap
    val charCount: Map[Char, Int] = string.foldLeft(Map[Char, Int]()) {
      case (map, newChar) => map + (newChar -> (map.getOrElse(newChar, 0) + 1))
    }

    if (charCount.values.exists(_ > (string.length + 1) / 2)) "" // impossible case
    else organizeTailrec(charCount)
  }

  def main(args: Array[String]): Unit = {
    println(reorganizeString("aaabc"))
    println(reorganizeString("aaaaaaab").isEmpty)
  }
}
