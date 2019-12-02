package com.rockthejvm.strings

import scala.annotation.tailrec

object ParenthesisProblems extends App {

  /*
    "()" => true
    "()()" => true
    "(())" => true
    ")(" => false
   */
  def hasValidParentheses(string: String): Boolean = {
    /*
      "(())" -> vpt("(())", 0)
      = vpt("())", 1)
      = vpt("))", 2)
      = vpt(")", 1)
      = vpt("", 0)
      = true

      "())" -> vpt("())", 0)
     = vpt("))", 1)
     = vpt(")", 0)
     = false

     Complexity: O(n)
     */
    @tailrec
    def validParensTailrec(remaining: String, openParens: Int): Boolean = {
      if (remaining.isEmpty) openParens == 0
      else if (openParens == 0 && remaining.head == ')') false
      else if (remaining.head == '(') validParensTailrec(remaining.tail, openParens + 1)
      else validParensTailrec(remaining.tail, openParens - 1)
    }

    validParensTailrec(string, 0)
  }

  def testValidParentheses() = {
    println(hasValidParentheses("()"))
    println(hasValidParentheses(")("))
    println(hasValidParentheses("()()"))
    println(hasValidParentheses("(())"))
    println(hasValidParentheses("())"))
    println(hasValidParentheses(")()"))
    println(hasValidParentheses("(()()(()))((((())())(())))"))
  }


  

}
