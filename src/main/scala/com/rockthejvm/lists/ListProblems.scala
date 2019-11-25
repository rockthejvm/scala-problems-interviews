package com.rockthejvm.lists

import scala.annotation.tailrec

sealed abstract class RList[+T] {
  /**
    * Standard functions
    */
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  /**
    * Easy problems
    */
  // get element at a given index
  def apply(index: Int): T

  // the size of the list
  def length: Int

  // reverse the list
  def reverse: RList[T]

  // concatenate another list to this one
  def ++[S >: T](anotherList: RList[S]): RList[S]

  // remove an element at a given index, return a NEW list
  def removeAt(index: Int): RList[T]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def toString: String = "[]"

  /**
    * Easy problems
    */
  // get element at a given index
  override def apply(index: Int): Nothing = throw new NoSuchElementException

  // the size of the list
  override def length: Int = 0

  // reverse the empty list
  override def reverse: RList[Nothing] = RNil

  // append another list
  def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList

  // remove an element
  override def removeAt(index: Int): RList[Nothing] = RNil
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false
  override def toString: String = {
    @tailrec
    def toStringTailrec(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailrec(remaining.tail, s"$result${remaining.head}, ")
    }

    "[" + toStringTailrec(this, "") + "]"
  }

  /**
    * Easy problems
    */
  // get element at a given index
  override def apply(index: Int): T = {
    /*
      [1,2,3,4,5].apply(2) = applyTailrec([1,2,3,4,5], 0)
      = applyTailrec([2,3,4,5], 1)
      = applyTailrec([3,4,5], 2)
      = 3

      Complexity of this algorithm?
      O(min(N, index))
     */
    @tailrec
    def applyTailrec(remaining: RList[T], currentIndex: Int): T = {
      if (currentIndex == index) remaining.head
      else applyTailrec(remaining.tail, currentIndex + 1)
    }

    if (index < 0) throw new NoSuchElementException
    else applyTailrec(this, 0)
  }

  // the size of the list
  override def length: Int = {
    /*
      [1,2,3,4,5].length = lengthTailrec([1,2,3,4,5], 0)
      = lengthTailrec([2,3,4,5], 1)
      = lengthTailrec([3,4,5], 2)
      = lengthTailrec([4,5], 3)
      = lengthTailrec([5], 4)
      = lengthTailrec([], 5)
      = 5

      Complexity: O(N)
     */
    @tailrec
    def lengthTailrec(remainingList: RList[T], accumulator: Int): Int = {
      if (remainingList.isEmpty) accumulator
      else lengthTailrec(remainingList.tail, accumulator + 1)
    }

    lengthTailrec(this, 0)
  }

  // reverse this list into a new list
  override def reverse: RList[T] = {
    /*
      [1,2,3,4].reverse = reverseTailrec([1,2,3,4], RNil)
      = reverseTailrec([2,3,4], [1])
      = reverseTailrec([3,4], [2,1])
      = reverseTailrec([4], [3,2,1])
      = reverseTailrec([], [4,3,2,1])
      = [4,3,2,1]

      Complexity: O(N)
     */
    @tailrec
    def reverseTailrec(remainingList: RList[T], result: RList[T]): RList[T] = {
      if (remainingList.isEmpty) result
      else reverseTailrec(remainingList.tail, remainingList.head :: result)
    }

    reverseTailrec(this, RNil)
  }

  // append another list
  def ++[S >: T](anotherList: RList[S]): RList[S] = {
    /*
      [1,2,3] ++ [4,5] = concatTailrec([4,5], [3,2,1])
      = concatTailrec([5], [4,3,2,1])
      = concatTailrec([], [5,4,3,2,1])
      = [5,4,3,2,1]

      Complexity: O(M + N)
      length of this list = N
      length of the other list = M
     */
    @tailrec
    def concatTailrec(remainingList: RList[S], acc: RList[S]): RList[S] = {
      if (remainingList.isEmpty) acc
      else concatTailrec(remainingList.tail, remainingList.head :: acc)
    }

    concatTailrec(anotherList, this.reverse).reverse
  }

  // remove an element
  override def removeAt(index: Int): RList[T] = {
    /*
      [1,2,3,4,5].removeAt(2) = removeAtTailrec([1,2,3,4,5], 0, [])
      = removeAtTailrec([2,3,4,5], 1, [1])
      = removeAtTailrec([3,4,5], 2, [2,1])
      = [2,1].reverse ++ [4,5]

      Complexity: O(N)
     */
    @tailrec
    def removeAtTailrec(remaining: RList[T], currentIndex: Int, predecessors: RList[T]): RList[T] = {
      if (currentIndex == index) predecessors.reverse ++ remaining.tail
      else if (remaining.isEmpty) predecessors.reverse
      else removeAtTailrec(remaining.tail, currentIndex + 1, remaining.head :: predecessors)
    }

    if (index < 0) this
    else removeAtTailrec(this, 0, RNil)
  }

}

object RList {
  def from[T](iterable: Iterable[T]): RList[T] = {
    def convertToRListTailrec(remaining: Iterable[T], acc: RList[T]): RList[T] = {
      if (remaining.isEmpty) acc
      else convertToRListTailrec(remaining.tail, remaining.head :: acc)
    }

    convertToRListTailrec(iterable, RNil).reverse
  }
}

object ListProblems extends App {

  val aSmallList = 1 :: 2 :: 3 :: RNil // RNil.::(3).::(2).::(1)
  val aLargeList = RList.from(1 to 10000)

  // test get-kth
  println(aSmallList.apply(0))
  println(aSmallList.apply(2))
  println(aLargeList.apply(8735))

  // test length
  println(aSmallList.length)
  println(aLargeList.length)

  // test reverse
  println(aSmallList.reverse)
  println(aLargeList.reverse)

  // test concat
  println(aSmallList ++ aLargeList)

  // test removeAt
  println(aLargeList.removeAt(13))

}
