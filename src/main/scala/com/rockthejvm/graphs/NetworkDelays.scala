package com.rockthejvm.graphs

import com.rockthejvm.graphs.GraphProblems.Graph

import scala.annotation.tailrec

object NetworkDelays {

  /*
    n = number of nodes
    times = list((a,b,t))
    (a,b,t) = time between node a and node b is t

    What is the time it takes for the signal to go from the source to ALL the other nodes in the network?
   */
  def computeNetworkDelay(n: Int, times: List[(Int, Int, Int)], source: Int): Int = ???

  def main(args: Array[String]): Unit = {
    println(computeNetworkDelay(2, List((1,2,1)), 2)) // -1
    println(computeNetworkDelay(2, List((1,2,1)), 1)) // 1
    println(computeNetworkDelay(4, List((2,1,1), (2,3,1), (3,4,1)), 2)) // 2
    println(computeNetworkDelay(4, List((1,2,3), (1,3,10), (1,4,10), (2,3,4), (3,4,2)), 1)) // 9
  }
}
