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
  def mirror: BTree[T]

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
  def sameShapeAs[S >: T](that: BTree[S]): Boolean

  // tree is symmetrical with respect to the root node
  /*
        _____1_____
       /           \
     __2__       __6__
    /     \     /     \
    3     4     7     8
   */
  def isSymmetrical: Boolean
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

  // mirror
  override def mirror: BTree[Nothing] = BEnd

  // structure comparison
  override def sameShapeAs[S >: Nothing](that: BTree[S]): Boolean = that.isEmpty

  // symmetrical
  override def isSymmetrical: Boolean = true
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

  // mirror/swap children inside the tree
  override def mirror: BTree[T] = {
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
    def mirrorTailrec(todo: List[BTree[T]], expanded: Set[BTree[T]], done: List[BTree[T]]): BTree[T] = {
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
          val newNode = BNode(node.value, newLeft, newRight)
          mirrorTailrec(todo.tail, expanded, newNode :: done.drop(2))
        }
      }
    }

    mirrorTailrec(List(this), Set(), List())
  }

  // shape comparison
  override def sameShapeAs[S >: T](that: BTree[S]): Boolean = {
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
    def sameShapeAsTailrec(thisRemaining: List[BTree[S]], thatRemaining: List[BTree[S]]): Boolean = {
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

  val tree10x = BNode(10,
    BNode(20,
      BNode(30, BEnd, BEnd),
      BNode(40,
        BEnd,
        BNode(50, BEnd, BEnd),
      )
    ),
    BNode(60,
      BNode(70, BEnd, BEnd),
      BNode(80, BEnd, BEnd)
    )
  )

  val tree10xExtra = BNode(10,
    BNode(20,
      BNode(30, BEnd, BEnd),
      BNode(40,
        BEnd,
        BEnd
      )
    ),
    BNode(60,
      BNode(70, BEnd, BEnd),
      BNode(80, BEnd, BEnd)
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

  // mirroring
  println(tree.mirror)

  // same shape as
  println(tree.sameShapeAs(tree10x))
  println(tree.sameShapeAs(tree10xExtra))
  // symmetry
  println(tree10xExtra.isSymmetrical)
}
