package com.rockthejvm.strings

import scala.collection.immutable.TreeMap

object ReorganizeString {

  // "aaabc" -> "abaca", "acaba"
  // "aaa" -> ""
  // rearrange chars so that no two adjacent chars are identical
  def reorganizeString(string: String): String = ???

  def main(args: Array[String]): Unit = {
    println(reorganizeString("aaabc"))
    println(reorganizeString("aaaaaaab").isEmpty)
  }
}
