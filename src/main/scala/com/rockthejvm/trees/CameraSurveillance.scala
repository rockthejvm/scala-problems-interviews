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
  def cameraSurveillance[T](tree: Tree[T]): Int = {
    val COVERED = 0
    val NOT_COVERED = 1
    val CAMERA = 2

    /*
         >1
         / \
       >2   3
       /
      4

      (2, CAMERA)
        (1, CAMERA)
          (0, NOT_COVERED)
            (0, COVERED)
            (0, COVERED)
          (0, NOT_COVERED)
        (0, NOT_COVERED)
          (0, COVERED)
          (0, COVERED)

       Complexity: O(N) time, O(N) space
     */
    def minCamerasStack(node: Tree[T]): (Int, Int) = {
      if (node.isEmpty) (0, COVERED)
      else {
        val (leftNumCameras, leftState) = minCamerasStack(node.left)
        val (rightNumCameras, rightState) = minCamerasStack(node.right)

        /*
          - left or right is NOT covered => place camera in this node
          - left or right HAVE CAMERAS => consider the node COVERED
          - consider the node NOT_COVERED
         */
        if (leftState == NOT_COVERED || rightState == NOT_COVERED) (leftNumCameras + rightNumCameras + 1, CAMERA)
        else if (leftState == CAMERA || rightState == CAMERA) (leftNumCameras + rightNumCameras, COVERED)
        else (leftNumCameras + rightNumCameras, NOT_COVERED)
      }
    }

    /*
         >1
         / \
       >2   3
       /
      4

      mct([1], [], []) =
      mct([2 3 1], [1], []) =
      mct([4 End 2 3 1], [1 2], []) =
      mct([End End 4 End 2 3 1], [1 2 4], []) =
      mct([End 4 End 2 3 1], [1 2 4], [(0,C)]) =
      mct([4 End 2 3 1], [1 2 4], [(0,C), (O,C)]) =
      mct([End 2 3 1], [1 2 4], [(0, N)]) =
      mct([2 3 1], [1 2 4], [(0, C), (0, N)]) =
      mct([3 1], [1 2 4], [(1, >)]) =
      mct([End End 3 1], [1 2 3 4], [(1, >)]) =
      mct([End 3 1], [1 2 3 4], [(0,C), (1, >)]) =
      mct([3 1], [1 2 3 4], [(0,C), (0,C), (1 >)]) =
      mct([1], [1 2 3 4], [(0, N), (1, >)]) =
      mct([], [1 2 3 4], [(2, >)]) =
      (2, >)
     */
    def minCamerasTail(stack: List[Tree[T]], visited: Set[Tree[T]], coverageStack: List[(Int, Int)]): (Int, Int) =
      if (stack.isEmpty) coverageStack.head
      else {
        val node = stack.head

        if (node.isEmpty)
          minCamerasTail(stack.tail, visited, (0, COVERED) :: coverageStack)
        else if (!visited.contains(node))
          minCamerasTail(node.left :: node.right :: stack, visited + node, coverageStack)
        else {
          val (leftNumCameras, leftState) = coverageStack.head
          val (rightNumCameras, rightState) = coverageStack.tail.head

          val parentState =
            if (leftState == NOT_COVERED || rightState == NOT_COVERED) (leftNumCameras + rightNumCameras + 1, CAMERA)
            else if (leftState == CAMERA || rightState == CAMERA) (leftNumCameras + rightNumCameras, COVERED)
            else (leftNumCameras + rightNumCameras, NOT_COVERED)

          minCamerasTail(stack.tail, visited, parentState :: coverageStack.tail.tail)
        }
      }



    val (tailNumCameras, tailRootState) = minCamerasTail(List(tree), Set(), List())
    if (tailRootState == NOT_COVERED) tailNumCameras + 1 // additional camera in the root
    else tailNumCameras // root is covered

  }

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
