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
  override def isLeaf: Boolean = false
  override def collectLeaves: List[Tree[Nothing]] = List()
  override def leafCount: Int = 0

  /**
    * Medium difficulty problems
    */
  // the number of nodes in the tree
  override val size: Int = 0

  // nodes at a given level
  override def collectNodes(level: Int): List[Tree[Nothing]] = List()

  // mirror
  override def mirror: Tree[Nothing] = End

  // structure comparison
  override def sameShapeAs[S >: Nothing](that: Tree[S]): Boolean = that.isEmpty

  // symmetrical
  override def isSymmetrical: Boolean = true

  // collect nodes to list
  override def toList: List[Nothing] = List()
}

case class Node[+T](override val value: T, override val left: Tree[T], override val right: Tree[T]) extends Tree[T] {
  override def isEmpty: Boolean = false

  /**
    * Easy problems
    */
  override def isLeaf: Boolean = left.isEmpty && right.isEmpty

  override def collectLeaves: List[Tree[T]] = {
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
    def collectLeavesTailrec(todo: List[Tree[T]], leaves: List[Tree[T]]): List[Tree[T]] = {
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
  override def collectNodes(level: Int): List[Tree[T]] = {
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
    def collectNodesTailrec(currentLevel: Int, currentNodes: List[Tree[T]]): List[Tree[T]] = {
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

  // mirror/swap children inside the tree
  override def mirror: Tree[T] = {
    /*
        _____1_____                     _____1_____
       /           \                   /           \
     __2__       __6__       ->      __6__       __2__
    /     \     /     \             /     \     /     \
    3     4     7     8             8     7     4     3
           \                                   /
            5                                 5

    mt([1], [], []) =
    mt([2,6,1], [1], []) =
    mt([3,4,2,6,1], [1,2], []) =
    mt([4,2,6,1], [1,2], [3]) =
    mt([End, 5, 4,2,6,1], [1,2,4], [3]) =
    mt([5,4,2,6,1], [1,2,4], [End, 3]) =
    mt([4,2,6,1], [1,2,4], [5, End, 3]) =
    mt([2,6,1], [1,2,4], [(4 5 End), 3]) =
    mt([6,1], [1,2,4], [(2 (4 5 End) 3)] =
    mt([7,8,6,1], [1,2,4,6], [(2 (4 5 End) 3)]) =
    mt([8,6,1], [1,2,4,6], [7, (2 (4 5 End) 3)]) =
    mt([6,1], [1,2,4,6], [8,7, (2 (4 5 End) 3)]) =
    mt([1], [1,2,4,6], [(6 8 7), (2 (4 5 End) 3)]) =
    mt([], [1,2,4,6], [(1 (6 8 7) (2 (4 5 End) 3)]) =
    (1 (6 8 7) (2 (4 5 End) 3)

    Complexity: O(N)
     */
    @tailrec
    def mirrorTailrec(todo: List[Tree[T]], expanded: Set[Tree[T]], done: List[Tree[T]]): Tree[T] = {
      if (todo.isEmpty) done.head
      else {
        val node = todo.head
        if (node.isEmpty || node.isLeaf) {
          mirrorTailrec(todo.tail, expanded, node :: done)
        } else if (!expanded.contains(node)) {
          mirrorTailrec(node.left :: node.right :: todo, expanded + node, done)
        } else {
          val newLeft = done.head
          val newRight = done.tail.head
          val newNode = Node(node.value, newLeft, newRight)
          mirrorTailrec(todo.tail, expanded, newNode :: done.drop(2))
        }
      }
    }

    mirrorTailrec(List(this), Set(), List())
  }

  // shape comparison
  override def sameShapeAs[S >: T](that: Tree[S]): Boolean = {
    /*
        _____1_____                     _____8_____
       /           \                   /           \
     __2__       __6__       ~~      __9__       __2__
    /     \     /     \             /     \     /     \
    3     4     7     8             1     3     2     7
           \                               \
            5                               4

        sst([1], [8]) =
        sst([2,6], [9,2]) =
        sst([3,4,6], [1,3,2]) =
        sst([4,6],[3,2]) =
        sst([End, 5, 6], [End, 4, 2]) =
        sst([5,6], [4,2]) =
        sst([6], [2]) =
        sst([7,8], [2,7]) =
        sst([8], [7]) =
        sst([], []) =
        true

        Complexity: O(max(N1, N2))
     */
    @tailrec
    def sameShapeAsTailrec(thisRemaining: List[Tree[S]], thatRemaining: List[Tree[S]]): Boolean = {
      if (thisRemaining.isEmpty) thatRemaining.isEmpty
      else if (thatRemaining.isEmpty) thisRemaining.isEmpty
      else {
        val thisNode = thisRemaining.head
        val thatNode = thatRemaining.head

        if (thisNode.isEmpty) thatNode.isEmpty && sameShapeAsTailrec(thisRemaining.tail, thatRemaining.tail)
        else if (thisNode.isLeaf) thatNode.isLeaf && sameShapeAsTailrec(thisRemaining.tail, thatRemaining.tail)
        else sameShapeAsTailrec(
          thisNode.left :: thisNode.right :: thisRemaining.tail,
          thatNode.left :: thatNode.right :: thatRemaining.tail
        )
      }
    }

    sameShapeAsTailrec(List(this), List(that))
  }

  // symmetry
  override def isSymmetrical: Boolean = sameShapeAs(this.mirror)

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
  override def toList: List[T] = {
    def preOrderStack(tree: Tree[T]): List[T] =
      if (tree.isEmpty) List()
      else tree.value :: preOrderStack(tree.left) ++ preOrderStack(tree.right)

    /*
      pot([1], [], []) =
      pot([1 2 6], [1], []) =
      pot([2 6], [1], [1]) =
      pot([2 3 4 6], [1 2], [1]) =
      pot([3 4 6], [1 2], [1 2]) =
      pot([4 6], [1 2], [1 2 3] =
      pot([4 5 6], [1 2 4], [1 2 3]) =
      pot([5 6], [1 2 4], [1 2 3 4]) =
      pot([6], [1 2 4], [1 2 3 4 5]) =
      pot([6 7 8], [1 2 4 6], [1 2 3 4 5]) =
      pot([7 8], [1 2 4 6], [1 2 3 4 5 6]) =
      pot([8], [1 2 4 6], [1 2 3 4 5 6 7]) =
      pot([], [1 2 4 6], [1 2 3 4 5 6 7 8]) =
      [1 2 3 4 5 6 7 8]
     */
    @tailrec
    def preOrderTailrec(stack: List[Tree[T]], visited: Set[Tree[T]] = Set(), acc: Queue[T] = Queue()): List[T] =
      if (stack.isEmpty) acc.toList
      else {
        val node = stack.head
        if (node.isEmpty) preOrderTailrec(stack.tail, visited, acc)
        else if (node.isLeaf || visited.contains(node)) preOrderTailrec(stack.tail, visited, acc :+ node.value)
        else preOrderTailrec(node :: node.left :: node.right :: stack.tail, visited + node, acc)
      }

    /*
      plt([1], []) =
      plt([2, 6], [1]) =
      plt([3,4,7,8], [1 2 6]) =
      plt([5], [1 2 6 3 4 7 8]) =
      plt([], [1 2 6 3 4 7 8 5]) =
      [1 2 6 3 4 7 8 5]
     */
    @tailrec
    def perLevelTailrec(level: List[Tree[T]], finalQueue: Queue[Tree[T]] = Queue()): List[T] =
      if (level.isEmpty) finalQueue.map(_.value).toList
      else perLevelTailrec(
        level.flatMap(node => List(node.left, node.right).filter(!_.isEmpty)),
        finalQueue ++ level
      )

    perLevelTailrec(List(this))
  }
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
