package com.rockthejvm.trees

import scala.annotation.tailrec
import scala.collection.immutable.Queue

sealed abstract class Tree[+T] {
  def value: T
  def left: Tree[T]
  def right: Tree[T]
  def isEmpty: Boolean

  /**
    * Easy problems
    */
  def isLeaf: Boolean
  def collectLeaves: List[Tree[T]]
  def leafCount: Int

  /**
    * Medium difficulty problems
    */
  // the number of nodes in the tree
  def size: Int

  // nodes at a given level
  def collectNodes(level: Int): List[Tree[T]]

  // mirror a tree
  /*
        _____1_____                     _____1_____
       /           \                   /           \
     __2__       __6__       ->      __6__       __2__
    /     \     /     \             /     \     /     \
    3     4     7     8             8     7     4     3
           \                                   /
            5                                 5
   */
  def mirror: Tree[T]

  // compare the shape of two trees
  /*
        _____1_____                     _____8_____
       /           \                   /           \
     __2__       __6__       ~~      __9__       __2__
    /     \     /     \             /     \     /     \
    3     4     7     8             1     3     2     7
           \                               \
            5                               4
  */
  def sameShapeAs[S >: T](that: Tree[S]): Boolean

  // tree is symmetrical with respect to the root node
  /*
        _____1_____
       /           \
     __2__       __6__
    /     \     /     \
    3     4     7     8
   */
  def isSymmetrical: Boolean

  // collect all nodes to a list
  def toList: List[T]
}

case object End extends Tree[Nothing] {
  override def value: Nothing = throw new NoSuchElementException
  override def left: Tree[Nothing] = throw new NoSuchElementException
  override def right: Tree[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true

  /**
    * Easy problems
    */
  override def isLeaf: Boolean = ???
  override def collectLeaves: List[Tree[Nothing]] = ???
  override def leafCount: Int = ???

  /**
    * Medium difficulty problems
    */
  // the number of nodes in the tree
  override val size: Int = ???

  // nodes at a given level
  override def collectNodes(level: Int): List[Tree[Nothing]] = ???

  // mirror
  override def mirror: Tree[Nothing] = ???

  // structure comparison
  override def sameShapeAs[S >: Nothing](that: Tree[S]): Boolean = ???

  // symmetrical
  override def isSymmetrical: Boolean = ???

  // collect nodes to list
  override def toList: List[Nothing] = ???
}

case class Node[+T](override val value: T, override val left: Tree[T], override val right: Tree[T]) extends Tree[T] {
  override def isEmpty: Boolean = false

  /**
    * Easy problems
    */
  override def isLeaf: Boolean = ???

  override def collectLeaves: List[Tree[T]] = ???

  override def leafCount: Int = ???

  /**
    * Medium difficulty problems
    */
  // the number of nodes in the tree
  override val size: Int = ???

  // nodes at a given level
  override def collectNodes(level: Int): List[Tree[T]] = ???

  // mirror/swap children inside the tree
  override def mirror: Tree[T] = ???

  // shape comparison
  override def sameShapeAs[S >: T](that: Tree[S]): Boolean = ???

  // symmetry
  override def isSymmetrical: Boolean = ???

  // collect to list
  /*

            _____1_____
           /           \
         __2__       __6__
        /     \     /     \
        3     4     7     8
               \
                5

    Options:
    - pre-order: [1 2 3 4 5 6 7 8]
    - in-order: [3 2 4 5 1 7 6 8]
    - post-order: [3 5 4 2 7 6 8 1]
    - per-level: [1 2 6 3 4 7 8 5]
   */
  override def toList: List[T] = ???
}

object Tree {
  def apply[T](): Tree[T] = End
  def apply[T](value: T): Tree[T] = Node(value, End, End)
  def apply[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Node(value, left, right)
}

object BinaryTreeProblems extends App {

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

  val tree10x = Node(10,
    Node(20,
      Node(30, End, End),
      Node(40,
        End,
        Node(50, End, End),
      )
    ),
    Node(60,
      Node(70, End, End),
      Node(80, End, End)
    )
  )

  val tree10xExtra = Node(10,
    Node(20,
      Node(30, End, End),
      Node(40,
        End,
        End
      )
    ),
    Node(60,
      Node(70, End, End),
      Node(80, End, End)
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
  val degenerate = (1 to 100000).foldLeft[Tree[Int]](End)((tree, number) => Node(number, tree, End))
  println(degenerate.size)

  // collect nodes at a given level
  println(tree.collectNodes(0).map(_.value))
  println(tree.collectNodes(2).map(_.value))
  println(tree.collectNodes(6473).map(_.value))

  // mirroring
  println(tree.mirror)

  // same shape as
  println(tree.sameShapeAs(tree10x))
  println(tree.sameShapeAs(tree10xExtra))
  // symmetry
  println(tree10xExtra.isSymmetrical)
  // collect nodes as list
  println(tree.toList)
}
