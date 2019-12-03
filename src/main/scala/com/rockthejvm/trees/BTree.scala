package com.rockthejvm.trees

import scala.annotation.tailrec

sealed abstract class BTree[+T] {
  def value: T
  def left: BTree[T]
  def right: BTree[T]
  def isEmpty: Boolean

  /**
    * Easy problems
    */
  def isLeaf: Boolean
  def collectLeaves: List[BTree[T]]
  def leafCount: Int

  /**
    * Medium difficulty problems
    */
  // the number of nodes in the tree
  def size: Int

  // nodes at a given level
  def collectNodes(level: Int): List[BTree[T]]
}

case object BEnd extends BTree[Nothing] {
  override def value: Nothing = throw new NoSuchElementException
  override def left: BTree[Nothing] = throw new NoSuchElementException
  override def right: BTree[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true

  /**
    * Easy problems
    */
  override def isLeaf: Boolean = false
  override def collectLeaves: List[BTree[Nothing]] = List()
  override def leafCount: Int = 0

  /**
    * Medium difficulty problems
    */
  // the number of nodes in the tree
  override val size: Int = 0

  // nodes at a given level
  override def collectNodes(level: Int): List[BTree[Nothing]] = List()
}

case class BNode[+T](override val value: T, override val left: BTree[T], override val right: BTree[T]) extends BTree[T] {
  override def isEmpty: Boolean = false

  /**
    * Easy problems
    */
  override def isLeaf: Boolean = left.isEmpty && right.isEmpty

  override def collectLeaves: List[BTree[T]] = {
    /*

        _____1_____
       /           \
     __2__       __6__
    /     \     /     \
    3     4     7     8
           \
            5

       clt([1], []) =
       clt([2, 6], []) =
       clt([3,4,6], []) =
       clt([4,6], [3]) =
       clt([5,6], [3]) =
       clt([6], [5,3]) =
       clt([7,8], [5,3]) =
       clt([8], [7,5,3]) =
       clt([], [8,7,5,3]) =
       [8,7,5,3]

     */
    @tailrec
    def collectLeavesTailrec(todo: List[BTree[T]], leaves: List[BTree[T]]): List[BTree[T]] = {
      if (todo.isEmpty) leaves
      else if (todo.head.isEmpty) collectLeavesTailrec(todo.tail, leaves)
      else if (todo.head.isLeaf) collectLeavesTailrec(todo.tail, todo.head :: leaves)
      else {
        val node = todo.head
        collectLeavesTailrec(node.left :: node.right :: todo.tail, leaves)
      }
    }

    collectLeavesTailrec(List(this), List())
  }

  override def leafCount: Int = collectLeaves.length

  /**
    * Medium difficulty problems
    */
  // the number of nodes in the tree
  override val size: Int = 1 + left.size + right.size

  // nodes at a given level
  override def collectNodes(level: Int): List[BTree[T]] = {
    /*
            _____1_____
           /           \
         __2__       __6__
        /     \     /     \
        3     4     7     8
               \
                5

        level = 2

       cnt(0, [{1}])
       = cnt(1, [{2}, {6}])
       = cnt(2, [{3}, {4}, {7}, {8}])
       = [{3}, {4}, {7}, {8}]
     */
    @tailrec
    def collectNodesTailrec(currentLevel: Int, currentNodes: List[BTree[T]]): List[BTree[T]] = {
      if (currentNodes.isEmpty) List()
      else if (currentLevel == level) currentNodes
      else {
        val expandedNodes = for {
          node <- currentNodes
          child <- List(node.left, node.right) if !child.isEmpty
        } yield child

        collectNodesTailrec(currentLevel + 1, expandedNodes)
      }
    }

    if (level < 0) List()
    else collectNodesTailrec(0, List(this))
  }

}

object BinaryTreeProblems extends App {

  val tree = BNode(1,
    BNode(2,
      BNode(3, BEnd, BEnd),
      BNode(4,
        BEnd,
        BNode(5, BEnd, BEnd),
      )
    ),
    BNode(6,
      BNode(7, BEnd, BEnd),
      BNode(8, BEnd, BEnd)
    )
  )

  /**
    * Easy problems
    */
  println(tree.collectLeaves.map(_.value))
  println(tree.leafCount)

  /**
    * Medium difficulty problems
    */

  // tree size
  val degenerate = (1 to 100000).foldLeft[BTree[Int]](BEnd)((tree, number) => BNode(number, tree, BEnd))
  println(degenerate.size)

  // collect nodes at a given level
  println(tree.collectNodes(0).map(_.value))
  println(tree.collectNodes(2).map(_.value))
  println(tree.collectNodes(6473).map(_.value))
}
