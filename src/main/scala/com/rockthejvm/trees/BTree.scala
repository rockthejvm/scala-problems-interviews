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

  val degenerate = (1 to 100000).foldLeft[BTree[Int]](BEnd)((tree, number) => BNode(number, tree, BEnd))
  println(degenerate.size)
}
