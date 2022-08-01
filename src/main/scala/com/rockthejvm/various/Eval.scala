package com.rockthejvm.various

import scala.annotation.tailrec


object Eval extends App {

  def eval(expr: String): Int = ???

  println(eval("1 + 2"))
  println(eval("1 + 2 + 3"))
  println(eval("1 + 2 * 3"))
  println(eval("1 * 2 + 3"))
  println(eval("1 - 2"))
  println(eval("1 - 2 * 3"))
  println(eval("1 + 2 * 3 + 4 / 5 + 6 * 7 - 8"))
}
