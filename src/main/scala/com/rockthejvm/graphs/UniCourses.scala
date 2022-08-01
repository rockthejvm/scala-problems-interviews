package com.rockthejvm.graphs

import com.rockthejvm.graphs.GraphProblems.{Graph, findCycle}

object UniCourses {

  /*
    nCourses courses at uni, labeled 0 -> n-1
    prerequisites = List[(a,b)]
    (a,b) = b is required in order to take a

    Can you take all courses 0 .. n-1 without breaking any prerequisite?
   */
  def canTakeAllCourses(nCourses: Int, prerequisites: List[(Int, Int)]): Boolean = ???

  // topological sort
  def findOrder(n: Int, prerequisites: List[(Int, Int)]): List[Int] = ???

  def main(args: Array[String]): Unit = {
    println(canTakeAllCourses(2, List((0, 1)))) // true
    println(canTakeAllCourses(2, List((0, 1), (1, 0)))) // false
    println(canTakeAllCourses(6, List((0, 1), (2, 0), (3, 0), (4, 1), (5, 4)))) // true
    println(findOrder(2, List((0, 1)))) // List(1, 0)
    println(findOrder(3, List((0, 1), (1, 2), (2, 0)))) // List()
    println(findOrder(6, List((0, 1), (2, 0), (3, 0), (4, 1), (5, 4)))) // List(1, 4, 5, 0, 3, 2)
  }
}
