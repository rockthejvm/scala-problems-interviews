package com.rockthejvm.strings

import scala.annotation.tailrec

object ParenthesisProblems extends App {

  /*
    "()" => true
    "()()" => true
    "(())" => true
    ")(" => false
   */
  def hasValidParentheses(string: String): Boolean = ???

  def testValidParentheses() = {
    println(hasValidParentheses("()"))
    println(hasValidParentheses(")("))
    println(hasValidParentheses("()()"))
    println(hasValidParentheses("(())"))
    println(hasValidParentheses("())"))
    println(hasValidParentheses(")()"))
    println(hasValidParentheses("(()()(()))((((())())(())))"))
  }


  /*
    n = 1 => List("()")
    n = 2 => List("()()", "(())")
    n = 3 => List("()()()", "()(())", "(())()", "((()))", "(()())")
   */
  def generateAllValidParentheses(n: Int): List[String] = ???

  def testGenParens() = {
    println(generateAllValidParentheses(1))
    println(generateAllValidParentheses(2))
    println(generateAllValidParentheses(3))
    println(generateAllValidParentheses(10))
  }

  testGenParens()

}
