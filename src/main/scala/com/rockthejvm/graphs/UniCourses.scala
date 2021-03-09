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

  // topological sort
  def findOrder(n: Int, prerequisites: List[(Int, Int)]): List[Int] = {
    // (course -> set of courses that depend on it)
    // e.g. { 0 -> [2, 3], 1 -> [0, 4], 2 -> [], 3 -> [], 4 -> [5], 5 -> [] }
    val dependencies: Graph[Int] =
      (0 until n).map(course => (course, Set[Int]())).toMap ++
        prerequisites.foldLeft(Map[Int, Set[Int]]()) {
          case (map, (a, b)) => map + (b -> (map.getOrElse(b, Set()) + a))
        }

    /*
      { 0 -> [2, 3], 1 -> [0, 4], 2 -> [], 3 -> [], 4 -> [5], 5 -> [] }

      ot([0 1 2 3 4 5], [], [], [], []) =
      ot([1 2 3 4 5], [0], [], [], []) =
      ot([1 2 3 4 5], [2 3 0], [0], [], []) =
      ot([1 2 3 4 5], [3 0], [0], [2], [2]) =
      ot([1 2 3 4 5], [0], [0], [2 3], [3 2]) =
      ot([1 2 3 4 5], [], [], [0 2 3], [0 3 2]) =
      ot([2 3 4 5], [1], [], [0 2 3], [0 3 2]) =
      ot([2 3 4 5], [0 4 1], [1], [0 2 3], [0 3 2]) =
      ot([2 3 4 5], [4 1], [1], [0 2 3], [0 3 2]) =
      ot([2 3 4 5], [5 4 1], [4 1], [0 2 3], [0 3 2]) =
      ot([2 3 4 5], [4 1], [4 1], [0 2 3 5], [5 0 3 2]) =
      ot([2 3 4 5], [1], [1], [0 2 3 4 5], [4 5 0 3 2]) =
      ot([2 3 4 5], [], [], [0 1 2 3 4 5], [1 4 5 0 3 2]) =
      ot([3 4 5], [2], [], [0 1 2 3 4 5], [1 4 5 0 3 2]) =
      ot([3 4 5], [], [], [0 1 2 3 4 5], [1 4 5 0 3 2]) =
      ot([4 5], [3], [], [0 1 2 3 4 5], [1 4 5 0 3 2]) =
      ot([4 5], [], [], [0 1 2 3 4 5], [1 4 5 0 3 2]) =
      .
      .
      .
      ot([], [], [], [0 1 2 3 4 5], [1 4 5 0 3 2]) =
      [1 4 5 0 3 2]

      Complexity: O(N) time, O(N) space
     */
    def orderTailrec(
                      remainingCourses: Set[Int],
                      stack: List[Int],
                      expanding: Set[Int],
                      expanded: Set[Int],
                      order: List[Int]
                    ): List[Int] =
      if (stack.isEmpty)
        if (remainingCourses.isEmpty) order
        else orderTailrec(remainingCourses.tail, List(remainingCourses.head), Set(), expanded, order)
      else {
        val course = stack.head

        if (expanded.contains(course)) {
          // course is already in the final order
          orderTailrec(remainingCourses, stack.tail, expanding, expanded, order)
        } else if (expanding.contains(course)) {
          // course has already been expanded (DFS), include it in the order
          orderTailrec(remainingCourses, stack.tail, expanding - course, expanded + course, course :: order)
        } else {
          // expansion phase
          val coursesAfter = dependencies(course) // at least the empty set
          if (coursesAfter.exists(neighborCourse => expanding.contains(neighborCourse)))
            List() // there is a cycle in my graph
          else
            orderTailrec(remainingCourses, coursesAfter.toList ++ stack, expanding + course, expanded, order)
        }
      }

    orderTailrec(dependencies.keySet, List(), Set(), Set(), List())
  }

  def main(args: Array[String]): Unit = {
    println(canTakeAllCourses(2, List((0, 1)))) // true
    println(canTakeAllCourses(2, List((0, 1), (1, 0)))) // false
    println(canTakeAllCourses(6, List((0, 1), (2, 0), (3, 0), (4, 1), (5, 4)))) // true
    println(findOrder(2, List((0, 1)))) // List(1, 0)
    println(findOrder(3, List((0, 1), (1, 2), (2, 0)))) // List()
    println(findOrder(6, List((0, 1), (2, 0), (3, 0), (4, 1), (5, 4)))) // List(1, 4, 5, 0, 3, 2)
  }
}
