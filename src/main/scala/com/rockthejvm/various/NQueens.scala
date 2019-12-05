package com.rockthejvm.various

import scala.annotation.tailrec

object NQueens extends App {


  def nQueens(n: Int): List[String] = {
    def conflict(position: Int, queens: List[Int]): Boolean = {
      def conflictOneQueen(position: Int, queen: Int, index: Int): Boolean =
        queen == position || (index + 1) == (position - queen) || (index + 1) == (queen - position)

      queens.zipWithIndex.exists { pair =>
        val (queen, index) = pair
        conflictOneQueen(position, queen, index)
      }
    }

    /*
      ._._._._.
      |_|x|_|_|
      |_|_|_|_|
      |_|_|_|_|
      |_|_|_|_|

      nQueens(4)
      nqt(0, [], []) =
      nqt(0, [0], []) =
      nqt(1, [0], []) =
      nqt(2, [0], []) =
      nqt(0, [2,0], []) =
      nqt(1, [2,0], []) =
      nqt(2, [2,0], []) =
      nqt(3, [2,0], []) =
      nqt(4, [2,0], []) =
      nqt(3, [0], []) =
      nqt(0, [3,0], []) =
      nqt(1, [3,0], []) =
      nqt(0, [1,3,0], []) =
      nqt(1, [1,3,0], []) =
      nqt(2, [1,3,0], []) =
      nqt(3, [1,3,0], []) =
      nqt(4, [1,3,0], []) =
      nqt(2, [3,0], []) =
      nqt(3, [3,0], []) =
      nqt(4, [3,0], []) =
      nqt(4, [0], [])
      nqt(1, [], [])

      ._._._._.
      |_|x|_|_|
      |_|_|_|x|
      |x|_|_|_|
      |_|_|x|_|

      [1,3,0,2]
      ._._._._.
      |_|_|x|_|
      |x|_|_|_|
      |_|_|_|x|
      |_|x|_|_|

      [2,0,3,1]

     */
    @tailrec
    def nQueensTailrec(currentPosition: Int, currentQueens: List[Int], solutions: List[List[Int]]): List[List[Int]] = {
      // I'm out of options
      if (currentPosition >= n && currentQueens.isEmpty) solutions
      else if (currentPosition >= n) {
        // I'm out of options on THIS row; move the previous queen by 1
        nQueensTailrec(currentQueens.head + 1, currentQueens.tail, solutions)
      } else if (conflict(currentPosition, currentQueens)) {
        // conflict with the other queens, try next position
        nQueensTailrec(currentPosition + 1, currentQueens, solutions)
      } else if (currentQueens.length == n-1) {
        // I've just built a solution
        val newSolution = currentPosition :: currentQueens
        nQueensTailrec(currentPosition + 1, currentQueens, newSolution :: solutions)
      } else {
        // try next queen on the next row, as this one is valid
        nQueensTailrec(0, currentPosition :: currentQueens, solutions)
      }
    }

    def prettyPrint(solution: List[Int]): String = {
      val topEdge = (1 to n).map(_ => "_").mkString(".", ".", ".") // ._._._._.
      val rows = solution.map { queen =>
        val cellsBefore = (0 until queen).map(_ => "_")
        val beforeString = if (cellsBefore.isEmpty) "|" else cellsBefore.mkString("|", "|", "|")
        val cellsAfter = ((queen + 1) until n).map(_ => "_")
        val afterString = if (cellsAfter.isEmpty) "|" else cellsAfter.mkString("|", "|", "|")

        beforeString + "x" + afterString
      }

      s"$topEdge\n${rows.mkString("\n")}"
    }

    nQueensTailrec(0, List(), List()).map(prettyPrint)
  }

  val q8 = nQueens(8)
  val printableSolution = q8.mkString("\n\n")

  println(printableSolution)
  println(s"Total solutions: ${q8.length}")
}
