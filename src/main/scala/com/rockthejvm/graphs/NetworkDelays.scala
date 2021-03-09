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
  def computeNetworkDelay(n: Int, times: List[(Int, Int, Int)], source: Int): Int = {
    // "adjacency" list/graph
    val graph: Graph[Int] = times.foldLeft(Map[Int, Set[Int]]()) {
      case (map, (a,b,_)) => map + (a -> (map.getOrElse(a, Set()) + b))
    }

    // adjacency "matrix"
    val weights: Map[(Int, Int), Int] = times.map {
      case (a,b,t) => ((a,b), t)
    }.toMap

    /*
      times: [(1,2,3), (1,3,10), (1,4,10), (2,3,4), (3,4,2)]
      graph: { 1 -> [2 3 4], 2 -> [3], 3 -> [4], 4 -> [] }
      weights: { (1,2) -> 3, (1,3) -> 10, (1,4) -> 10, (2,3) -> 4, (3,4) -> 2 }
      source: 1

      dt([1], [], { 1 -> 0, 2 -> MAX, 3 -> MAX, 4 -> MAX }) =
      dt([2,3,4], [1], { 1 -> 0, 2 -> 3, 3 -> 10, 4 -> 10 }) =
      dt([3,4], [1,2], { 1 -> 0, 2 -> 3, 3 -> 7, 4 -> 10 }) =
      dt([4], [1,2,3], { 1 -> 0, 2 -> 3, 3 -> 7, 4 -> 9 }) =
      dt([], [1,2,3,4], { 1 -> 0, 2 -> 3, 3 -> 7, 4 -> 9 }) =
      { 1 -> 0, 2 -> 3, 3 -> 7, 4 -> 9 }

      Complexity: O(N^2)
     */
    @tailrec
    def dijkstraTailrec(expanding: Set[Int], visited: Set[Int], costs: Map[Int, Int]): Map[Int, Int] =
      if (expanding.isEmpty) costs
      else {
        val node = expanding.minBy(costs)

        val neighCosts: Map[Int, Int] = graph.getOrElse(node, Set()).map { neigh =>
          val currentCost = costs.getOrElse(neigh, Int.MaxValue)
          val tentativeCost = costs(node) + weights((node, neigh))
          val bestCost = Math.min(currentCost, tentativeCost)

          (neigh, bestCost)
        }.toMap

        val neighUnvisited = neighCosts.keySet.filterNot(visited) // all the nodes that are not visited

        dijkstraTailrec(expanding - node ++ neighUnvisited, visited + node, costs ++ neighCosts)
      }

    val initialCosts: Map[Int, Int] = (1 to n).map((_, Int.MaxValue)).toMap + (source -> 0)
    val latencies: Map[Int, Int] = dijkstraTailrec(Set(source), Set(), initialCosts)
    val maxLatency = latencies.values.max

    if (maxLatency == Int.MaxValue) -1 // it's impossible for all the nodes to receive the signal
    else maxLatency
  }

  def main(args: Array[String]): Unit = {
    println(computeNetworkDelay(2, List((1,2,1)), 2)) // -1
    println(computeNetworkDelay(2, List((1,2,1)), 1)) // 1
    println(computeNetworkDelay(4, List((2,1,1), (2,3,1), (3,4,1)), 2)) // 2
    println(computeNetworkDelay(4, List((1,2,3), (1,3,10), (1,4,10), (2,3,4), (3,4,2)), 1)) // 9
  }
}
