package com.rockthejvm.graphs

import com.rockthejvm.graphs.GraphProblems.{Graph, findCycle}

object UniCourses {

  /*
    nCourses courses at uni, labeled 0 -> n-1
    prerequisites = List[(a,b)]
    (a,b) = b is required in order to take a

    Can you take all courses 0 .. n-1 without breaking any prerequisite?
   */
  def canTakeAllCourses(nCourses: Int, prerequisites: List[(Int, Int)]): Boolean = {
    val dependencies: Graph[Int] =
      (0 until nCourses).map(course => (course, Set[Int]())).toMap ++
        prerequisites.foldLeft(Map[Int, Set[Int]]()) {
          case (map, (a, b)) => map + (a -> (map.getOrElse(a, Set()) + b))
        }

    (0 until nCourses).forall(course => findCycle(dependencies, course).isEmpty)
  }

  def main(args: Array[String]): Unit = {
    println(canTakeAllCourses(2, List((0, 1)))) // true
    println(canTakeAllCourses(2, List((0, 1), (1, 0)))) // false
    println(canTakeAllCourses(5, List((0, 1), (2, 0), (3, 0), (4, 1), (5, 4)))) // true
  }
}
