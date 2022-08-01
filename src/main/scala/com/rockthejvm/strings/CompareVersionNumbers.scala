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
  def compareVersionNumbers(version1: String, version2: String): Int = ???

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
