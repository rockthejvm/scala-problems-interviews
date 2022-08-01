package com.rockthejvm.trees

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object PathSum {

  /*
            _____1_____
           /           \
         __2__       __6__
        /     \     /     \
        3     4     7     8
               \
                5

        tree, 6 => true
        tree, 7 => false
   */
  // Return true if there is a path from root to a leaf, such that the sum of values is target.
  def hasPathSum(tree: Tree[Int], target: Int): Boolean = ???

  // all the paths from root to leaf such that the sum of values == target
  def findSumPaths(tree: Tree[Int], target: Int): List[List[Int]] = ???

  def main(args: Array[String]): Unit = {
    val tree = Node(1,
      Node(2,
        Node(3, End, End),
        Node(4,
          End,
          Node(5, End, End),
        )
      ),
      Node(6,
        Node(7, End, End),
        Node(8, End, End)
      )
    )

    println(hasPathSum(tree, 6)) // true
    println(hasPathSum(tree, 7)) // false
    println(hasPathSum(tree, 14)) // true
    println(hasPathSum(tree, 15)) // true
    println(hasPathSum(tree, 16)) // false
    println(hasPathSum(End, 0)) // true?

    val twoPathsto6 = Node(1,
      Node(2,
        Node(3, End, End),
        Node(4,
          End,
          Node(-1, End, End),
        )
      ),
      Node(6,
        Node(7, End, End),
        Node(8, End, End)
      )
    )

    println(findSumPaths(twoPathsto6, 6))
    println(findSumPaths(twoPathsto6, 7))
  }
}
