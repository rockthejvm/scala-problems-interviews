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
  def hasPathSum(tree: Tree[Int], target: Int): Boolean = {
    def hasPathSumStack(tree: Tree[Int], target: Int): Boolean =
      if (tree.isEmpty) target == 0
      else if (tree.isLeaf) target == tree.value
      else hasPathSumStack(tree.right, target - tree.value) || hasPathSumStack(tree.left, target - tree.value)

    /*
      hp([1], [6]) =
      hp([2 6], [5 5]) =
      hp([6 3 4], [5 3 3]) =
      hp([3 4 7 8], [3 3 -1 -1]) =
      true

      Complexity: O(N) time, O(N) memory
     */
    def hasPathSumTailrec(nodes: Queue[Tree[Int]], targets: Queue[Int]): Boolean =
      if (nodes.isEmpty) false
      else {
        val node = nodes.head
        val targetValue = targets.head
        val children = List(node.left, node.right).filter(!_.isEmpty)
        val childrenTargets = children.map(_ => targetValue - node.value)

        if (node.isLeaf && node.value == targetValue) true
        else hasPathSumTailrec(nodes.tail ++ children, targets.tail ++ childrenTargets)
      }
    if (tree.isEmpty) target == 0 else  // edge case for hasPathSumTailrec
    hasPathSumTailrec(Queue(tree), Queue(target))
  }

  // all the paths from root to leaf such that the sum of values == target
  def findSumPaths(tree: Tree[Int], target: Int): List[List[Int]] = {
    /*
            _____1_____
           /           \
         __2__       __6__
        /     \     /     \
        3     4     7     8
               \
                -1
       sp(1, 6) = [2 6].flatMap(f) = [[2 3] [2 4 -1]].map(path => 1 :: path) = [[1 2 3] [1 2 4 -1]]
         sp(2, 5) = [3, 4].flatMap(f) == [[3]].map(path => 2 :: path) ++ [[4 -1]].map(path => 2 :: path) = [[2 3] [2 4 -1]]
           sp(3, 3) = [[3]]
           sp(4, 3) = [[-1]].map(path => 4 :: path) = [[4, -1]]
             sp(-1, -1) = [[-1]]
         sp(6, 5) = []

     */
    def stackPaths(tree: Tree[Int], currentTarget: Int): List[List[Int]] =
      if (tree.isEmpty) List()
      else if (tree.isLeaf)
        if (currentTarget == tree.value) List(List(tree.value))
        else List()
      else List(tree.left, tree.right).filter(!_.isEmpty).flatMap { childNode =>
        val subPaths: List[List[Int]] = stackPaths(childNode, currentTarget - tree.value)
        subPaths.map(path => tree.value :: path)
      }


    /*
            _____1_____
           /           \
         __2__       __6__
        /     \     /     \
        3     4     7     8
               \
                -1

      tp([1], [6], [], [], []) =
      tp([2 6 1], [5 5 6], [1], [1], []) =
      tp([3 4 2 6 1], [3 3 5 5 6], [2 1], [1 2], []) =
      tp([4 2 6 1], [3 5 5 6], [2 1], [1 2], [[1 2 3]]) =
      tp([-1 4 2 6 1], [-1 3 5 5 6], [4 2 1], [1 2 4], [[1 2 3]]) =
      tp([4 2 6 1], [3 5 5 6], [4 2 1], [1 2 4], [[1 2 4 -1] [1 2 3]]) =
      tp([2 6 1], [5 5 6], [2 1], [1 2 4], [[1 2 4 -1] [1 2 3]]) =
      tp([6 1], [5 6], [1], [1 2 4], [[1 2 4 -1] [1 2 3]])
      ...
      tp([], [], [], [1 2 4], [[1 2 4 -1] [1 2 3]]) =
      [[1 2 4 -1] [1 2 3]]

      Complexity: O(N) time, O(N) space
     */
    @tailrec
    def tailPaths(
                   nodes: List[Tree[Int]],
                   targets: List[Int],
                   currentPath: List[Tree[Int]],
                   expanded: Set[Tree[Int]],
                   acc: List[List[Int]]
                 ): List[List[Int]] =
      if (nodes.isEmpty) acc
      else {
        val node = nodes.head
        val currentTarget = targets.head
        val children = List(node.left, node.right).filter(!_.isEmpty)
        val childrenTargets = children.map(_ => currentTarget - node.value)

        if (node.isLeaf)
          if (node.value == currentTarget)
            tailPaths(nodes.tail, targets.tail, currentPath, expanded, (node :: currentPath).reverse.map(_.value) :: acc)
          else
            tailPaths(nodes.tail, targets.tail, currentPath, expanded, acc)
        else
          if (expanded.contains(node))
            tailPaths(nodes.tail, targets.tail, currentPath.tail, expanded, acc)
          else
            tailPaths(children ++ nodes, childrenTargets ++ targets, node :: currentPath, expanded + node, acc)

      }

    tailPaths(List(tree), List(target), List(), Set(), List())
  }

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
