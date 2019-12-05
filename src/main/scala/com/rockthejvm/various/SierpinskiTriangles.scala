package com.rockthejvm.various

object SierpinskiTriangles extends App {

  /*
    n = 3
           *
          * *
         *   *
        * * * *
       *       *
      * *     * *
     *   *   *   *
    * * * * * * * *

    n = 2
           *
          * *
         *   *
        * * * *

    n = 1

           *
          * *

   */
  def sierpinski(n: Int): String = {
    def sierpinskiStack(level: Int): List[String] = {
      if (level == 0) List("*")
      else {
        val triangle = sierpinskiStack(level - 1)
        val spaces = " " * (1 << (level - 1)) // 2 ^ (n-1) spaces
        val topTriangle = triangle.map(spaces + _ + spaces)
        val bottomTriangles = triangle.map(row => row + " " + row)
        topTriangle ++ bottomTriangles
      }
    }

    def sierpinskiTail(currentLevel: Int, currentTriangle: List[String]): List[String] = {
      if (currentLevel >= n) currentTriangle
      else {
        val spaces = " " * (1 << currentLevel) // 2 ^ (n-1) spaces
        val topTriangle = currentTriangle.map(spaces + _ + spaces)
        val bottomTriangles = currentTriangle.map(row => row + " " + row)
        sierpinskiTail(currentLevel + 1, topTriangle ++ bottomTriangles)
      }
    }

    sierpinskiTail(0, List("*")).mkString("\n")
  }

  println(sierpinski(8))

}
