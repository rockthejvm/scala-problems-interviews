package com.rockthejvm.strings

object MultiplyStrings {

  // multiply two numbers represented as strings, of arbitrary length
  def multiplyStrings(a: String, b: String): String = {

    def multiplyByDigit(number: List[Int], factor: Int): List[Int] = {
      /*
        [3,2,1] * 6

        [3,2,1], 0, []
        [2,1], 1, [8]
        [1], 1, [3,8]
        [], 0, [7,3,8]
        => [8,3,7]
       */
      def multiplyByDigitTailrec(remainingDigits: List[Int], carry: Int, acc: List[Int]): List[Int] =
        if (remainingDigits.isEmpty)
          if (carry == 0) acc.reverse
          else (carry :: acc).reverse
        else {
          val newDigit = remainingDigits.head
          val newProduct = newDigit * factor + carry
          multiplyByDigitTailrec(remainingDigits.tail, newProduct / 10, (newProduct % 10) :: acc)
        }

      multiplyByDigitTailrec(number, 0, List())
    }

    /*
      32 + 1537
      [2,3] + [7,3,5,1]
      att([2,3], [7,3,5,1], 0, []) =
      att([3], [3,5,1], 0, [9]) =
      att([], [5,1], 0, [6,9]) =
      [9,6] ++ att([0], [5,1]) = [9,6] ++ [5,1] = [9,6,5,1] = 1569
     */
    def addTwoNumbers(a: List[Int], b: List[Int]): List[Int] = {
      def addTwoTailrec(remainingA: List[Int], remainingB: List[Int], carry: Int = 0, acc: List[Int] = List()): List[Int] = {
        if (remainingA.isEmpty && remainingB.isEmpty)
          if (carry == 0) acc.reverse
          else (carry :: acc).reverse
        else if (remainingA.isEmpty) acc.reverse ++ addTwoTailrec(List(carry), remainingB)
        else if (remainingB.isEmpty) acc.reverse ++ addTwoTailrec(List(carry), remainingA)
        else {
          val newSum = remainingA.head + remainingB.head + carry
          val newDigit = newSum % 10
          val newCarry = newSum / 10

          addTwoTailrec(remainingA.tail, remainingB.tail, newCarry, newDigit :: acc)
        }
      }

      if (a.isEmpty) b
      else if (b.isEmpty) a
      else addTwoTailrec(a, b)
    }

    // 123 * 456 -> List(3,2,1) * List(6,5,4)
    /*
        123 x
        456
        ---
        738
       6150
      49200
      -----
      56088

      100 x
        0
      ---
      000
   */
    def multiplyDigits(a: List[Int], b: List[Int]): List[Int] =
      b.zipWithIndex // List[(digit, index in b)]
        .map {
          case (digit, index) => List.fill(index)(0) ++ multiplyByDigit(a, digit) // partial result
        } // List[List[Int]] - contains all the partial results
        .reduce(addTwoNumbers)

    val digitsA = a.reverse.map(_ - '0').toList
    val digitsB = b.reverse.map(_ - '0').toList
    val digitsResult = multiplyDigits(digitsA, digitsB)

    val result = digitsResult.reverse.mkString("")

    if (result.isEmpty || result.charAt(0) == '0') "0"
    else result
  }

  def main(args: Array[String]): Unit = {
    println(multiplyStrings("123", "456"))
    println(multiplyStrings("125137859237859327893", "45652378957234896"))
  }
}
