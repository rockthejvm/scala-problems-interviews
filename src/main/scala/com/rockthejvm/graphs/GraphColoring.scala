package com.rockthejvm.graphs

import scala.annotation.tailrec

object GraphColoring {
  import GraphProblems._

  def color[T](graph: Graph[T]): Map[T, Int] = ???

  def testColor(): Unit = {
    val socialNetwork: Graph[String] = Map(
      "Alice" -> Set("Bob", "Charlie", "David"),
      "Bob" -> Set(),
      "Charlie" -> Set("David"),
      "David" -> Set("Bob", "Mary"),
      "Mary" -> Set("Bob", "Charlie")
    )
    println(color(socialNetwork))
  }

  def main(args: Array[String]): Unit = {
    testColor()
  }
}
