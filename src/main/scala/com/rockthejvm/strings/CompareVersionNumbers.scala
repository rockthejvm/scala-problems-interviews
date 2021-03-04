package com.rockthejvm.strings

object CompareVersionNumbers {

  /*
    Example: 0.9 < 1.0.3.4 < 1.1.0 < 2.0 < 2.1 == 2.01

    1.0 ... 1.0.0.0
    -1: version1 < version2
    0: version1 == version2
    1: version1 > version2

    Complexity: O(max(L1, L2)) time, O(L1 + L2) space
   */
  def compareVersionNumbers(version1: String, version2: String): Int = {
    def compareVersionsAsLists(revisionsV1: List[Int], revisionsV2: List[Int]): Int = {
      if (revisionsV1.isEmpty && revisionsV2.isEmpty) 0 // "same versions"
      else if (revisionsV1.isEmpty)
        if (revisionsV2.exists(_ != 0)) -1
        else 0
      else if (revisionsV2.isEmpty)
        if (revisionsV1.exists(_ != 0)) 1
        else 0
      else {
        val r1 = revisionsV1.head
        val r2 = revisionsV2.head

        if (r1 < r2) -1
        else if (r1 > r2) 1
        else compareVersionsAsLists(revisionsV1.tail, revisionsV2.tail)
      }
    }

    val sectionsV1 = version1.split("\\.").toList.map(_.toInt)
    val sectionsV2 = version2.split("\\.").toList.map(_.toInt)
    compareVersionsAsLists(sectionsV1, sectionsV2)
  }

  def main(args: Array[String]): Unit = {
    println(compareVersionNumbers("1", "1")) // 0
    println(compareVersionNumbers("1.0", "1")) // 0
    println(compareVersionNumbers("1.0", "1.0.0")) // 0
    println(compareVersionNumbers("0.9", "1.0.3.4")) // -1
    println(compareVersionNumbers("1.0.3.4", "1.1.0")) // -1
    println(compareVersionNumbers("1.1.0", "2.0")) // -1
    println(compareVersionNumbers("2.1", "2.0")) // 1
    println(compareVersionNumbers("2.1", "2.01")) // 0
  }
}
