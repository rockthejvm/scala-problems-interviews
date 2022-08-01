package com.rockthejvm.lists

import scala.annotation.tailrec
import scala.util.Random

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

  // the big 3
  def map[S](f: T => S): RList[S]
  def flatMap[S](f: T => RList[S]): RList[S]
  def filter(f: T => Boolean): RList[T]

  /**
    * Medium difficulty problems
    */
  // run-length encoding
  def rle: RList[(T, Int)]

  // duplicate each element a number of times in a row
  def duplicateEach(k: Int): RList[T]

  // rotation by a number of positions to the left
  def rotate(k: Int): RList[T]

  // random sample
  def sample(k: Int): RList[T]

  /**
    * Hard problems
    */
  // sorting the list in the order defined by the Ordering object
  def insertionSort[S >: T](ordering: Ordering[S]): RList[S]
  def mergeSort[S >: T](ordering: Ordering[S]): RList[S]
  def quickSort[S >: T](ordering: Ordering[S]): RList[S]
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
  override def apply(index: Int): Nothing = ???

  // the size of the list
  override def length: Int = ???

  // reverse the empty list
  override def reverse: RList[Nothing] = ???

  // append another list
  def ++[S >: Nothing](anotherList: RList[S]): RList[S] = ???

  // remove an element
  override def removeAt(index: Int): RList[Nothing] = ???

  // the big 3
  override def map[S](f: Nothing => S): RList[S] = ???
  override def flatMap[S](f: Nothing => RList[S]): RList[S] = ???
  override def filter(f: Nothing => Boolean): RList[Nothing] = ???

  /**
    * Medium difficulty problems
    */
  // run-length encoding
  override def rle: RList[(Nothing, Int)] = ???

  // duplicate each element a number of times in a row
  override def duplicateEach(k: Int): RList[Nothing] = ???

  // rotate by a number of positions to the left
  override def rotate(k: Int): RList[Nothing] = ???

  // random samples
  override def sample(k: Int): RList[Nothing] = ???

  /**
    * Hard problems
    */
  // sorting
  override def insertionSort[S >: Nothing](ordering: Ordering[S]): RList[S] = ???
  override def mergeSort[S >: Nothing](ordering: Ordering[S]): RList[S] = ???
  override def quickSort[S >: Nothing](ordering: Ordering[S]): RList[S] = ???
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
  override def apply(index: Int): T = ???

  // the size of the list
  override def length: Int = ???

  // reverse this list into a new list
  override def reverse: RList[T] = ???

  // append another list
  def ++[S >: T](anotherList: RList[S]): RList[S] = ???

  // remove an element
  override def removeAt(index: Int): RList[T] = ???

  // the big 3
  override def map[S](f: T => S): RList[S] = ???

  override def flatMap[S](f: T => RList[S]): RList[S] = ???

  override def filter(predicate: T => Boolean): RList[T] = ???

  /**
    * Medium difficulty problems
    */
  // run-length encoding
  override def rle: RList[(T, Int)] = ???

  // duplicate each element a number of times in a row
  override def duplicateEach(k: Int): RList[T] = ???

  // rotate by a number of positions to the left
  override def rotate(k: Int): RList[T] = ???

  // random samples
  override def sample(k: Int): RList[T] = ???

  /**
    * Hard problems
    */
  override def insertionSort[S >: T](ordering: Ordering[S]): RList[S] = ???

  override def mergeSort[S >: T](ordering: Ordering[S]): RList[S] = ???

  override def quickSort[S >: T](ordering: Ordering[S]): RList[S] = ???
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
  val oneToTen = RList.from(1 to 10)

  def testEasyFunctions() = {
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

    // map
    println(aLargeList.map(x => 2 * x))
    // flatMap
    val time = System.currentTimeMillis()
    aLargeList.flatMap(x => x :: (2 * x) :: RNil) // 1.3 seconds!
    println(System.currentTimeMillis() - time)
    // filter
    println(aLargeList.filter(x => x % 2 == 0))
  }

  def testMediumDifficultyFunctions() = {
    // run-length encoding
    println((1 :: 1 :: 1 :: 2 :: 3 :: 3 :: 4 :: 5 :: 5 :: 5 :: RNil).rle)

    // duplicateEach
    println(aSmallList.duplicateEach(4))

    // rotate
    for {
      i <- 1 to 20
    } println(oneToTen.rotate(i))

    // random samples
    println(aLargeList.sample(10))

    // better flatMap
    println(aSmallList.flatMap(x => x :: (2 * x) :: RNil))
    val time = System.currentTimeMillis()
    aLargeList.flatMap(x => x :: (2 * x) :: RNil) // 7 ms
    println(System.currentTimeMillis() - time)
  }

  def testHardFunctions() = {
    val anUnorderedList = 3 :: 1 :: 2 :: 4 :: 5 :: RNil
    val ordering = Ordering.fromLessThan[Int](_ < _)
    val listToSort = aLargeList.sample(10)

    // insertion sort
    println(anUnorderedList.insertionSort(ordering))
    println(listToSort.insertionSort(ordering))
    // merge sort
    println(listToSort.mergeSort(ordering))
    // quick sort
    println(listToSort.quickSort(ordering))
  }

  testHardFunctions()
}
