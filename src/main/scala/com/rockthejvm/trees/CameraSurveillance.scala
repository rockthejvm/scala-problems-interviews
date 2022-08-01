package com.rockthejvm.trees

object CameraSurveillance {

  /*
    Given a binary tree, we install cameras on the nodes of the tree.
    Each camera at a node can monitor its parent, itself, and its immediate children.
    Calculate the minimum number of cameras needed to monitor all nodes of the tree.

          1
         / \
        2   3
       /
      4
      => 2

              ______1______ <--
             /             \
           __2__         __3__
          /     \       /     \
         >4    >5       6<    7<
         / \     \     / \     \
       >8   9    10   11  12<  13
       /                   \
      14                   15

      => 7

              ______1______
             /             \
           __2__         __3__
          /     \       /     \
         >4    >5      >6    >7
         / \     \     / \     \
       >8   9    10   11 >12   13
       /                   \
      14                   15

   */
  def cameraSurveillance[T](tree: Tree[T]): Int = ???

  def main(args: Array[String]): Unit = {
    val smallTree =
      Tree(1,
        Tree(2,
          Tree(4),
          Tree()
        ),
        Tree(3)
      )

    val biggerTree =
      Tree(1,
        Tree(2,
          Tree(4,
            Tree(8,
              Tree(14),
              Tree()
            ),
            Tree(9)
          ),
          Tree(5,
            Tree(),
            Tree(10)
          )
        ),
        Tree(3,
          Tree(6,
            Tree(11),
            Tree(12,
              Tree(),
              Tree(15)
            ),
          ),
          Tree(7,
            Tree(),
            Tree(13)
          )
        )
      )

    println(cameraSurveillance(smallTree)) // 2
    println(cameraSurveillance(biggerTree)) // 7
  }
}
