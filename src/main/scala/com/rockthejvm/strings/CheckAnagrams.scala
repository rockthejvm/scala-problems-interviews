package com.rockthejvm.strings

object CheckAnagrams {

  import CountCharacters._

  def checkAnagrams(sa: String, sb: String): Boolean = countCharacters(sa) == countCharacters(sb)
  def checkAnagrams2(sa: String, sb: String): Boolean = sa.sorted == sb.sorted

  def testCheckAnagrams() = {
    println(checkAnagrams("desserts", "stressed"))
    println(checkAnagrams("Scala", "Haskell"))
    println(checkAnagrams2("desserts", "stressed"))
    println(checkAnagrams2("Scala", "Haskell"))
  }

  def main(args: Array[String]): Unit = {
    testCheckAnagrams()
  }
}
