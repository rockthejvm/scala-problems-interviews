package com.rockthejvm.graphs

object TownJudge {

  /*
    n people, 1 to n
    trust = List[(a, b)]
    (a,b) = a trusts b

    There might be a town judge.
      The town judge trusts nobody. == outDegree(tj) = 0
      Everybody (except for the town judge) trusts the town judge. == inDegree(tj) = n-1
      There is exactly one person that satisfies these properties.

    Find the town judge, or return -1.
   */
  def findJudge(n: Int, trust: List[(Int, Int)]): Int = ???

  def main(args: Array[String]): Unit = {
    println(findJudge(2, List((1, 2)))) // 2
    println(findJudge(3, List((1, 2), (3, 2)))) // 2
    println(findJudge(3, List((1, 2), (2, 3), (3, 1)))) // -1
    println(findJudge(4, List((1, 3), (2, 3), (1, 4), (2, 4), (3, 4)))) // 4
  }
}
