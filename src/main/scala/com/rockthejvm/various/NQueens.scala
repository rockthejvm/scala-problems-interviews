package com.rockthejvm.various

import scala.annotation.tailrec

object NQueens extends App {


  def nQueens(n: Int): List[String] = ???

  val q8 = nQueens(8)
  val printableSolution = q8.mkString("\n\n")

  println(printableSolution)
  println(s"Total solutions: ${q8.length}")
}
