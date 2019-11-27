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
  override def apply(index: Int): Nothing = throw new NoSuchElementException

  // the size of the list
  override def length: Int = 0

  // reverse the empty list
  override def reverse: RList[Nothing] = RNil

  // append another list
  def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList

  // remove an element
  override def removeAt(index: Int): RList[Nothing] = RNil

  // the big 3
  override def map[S](f: Nothing => S): RList[S] = RNil
  override def flatMap[S](f: Nothing => RList[S]): RList[S] = RNil
  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil

  /**
    * Medium difficulty problems
    */
  // run-length encoding
  override def rle: RList[(Nothing, Int)] = RNil

  // duplicate each element a number of times in a row
  override def duplicateEach(k: Int): RList[Nothing] = RNil

  // rotate by a number of positions to the left
  override def rotate(k: Int): RList[Nothing] = RNil

  // random samples
  override def sample(k: Int): RList[Nothing] = RNil

  /**
    * Hard problems
    */
  // sorting
  override def insertionSort[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil
  override def mergeSort[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil
  override def quickSort[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil
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

  // the big 3
  override def map[S](f: T => S): RList[S] = {
    /*
      [1,2,3].map(x => x + 1) = mapTailrec([1,2,3], [])
      = mapTailrec([2,3], [2])
      = mapTailrec([3], [3, 2])
      = mapTailrec([], [4,3,2])
      = [4,3,2].reverse
      = [2,3,4]

      Complexity: O(N)
     */
    @tailrec
    def mapTailrec(remaining: RList[T], accumulator: RList[S]): RList[S] = {
      if (remaining.isEmpty) accumulator.reverse
      else mapTailrec(remaining.tail, f(remaining.head) :: accumulator)
    }

    mapTailrec(this, RNil)
  }

  override def flatMap[S](f: T => RList[S]): RList[S] = {
    /*
      [1,2,3].flatMap(x => [x, 2 * x]) = fmTailrec([1,2,3], [])
      = fmTailrec([2,3], [1,2].reverse)
      = fmTailrec([3], [2,4].reverse ++ [1,2].reverse)
      = fmTailrec([], [3,6].reverse ++ [2,4].reverse ++ [1,2].reverse)
      = [6,3,4,2,2,1].reverse
      = [1,2,2,4,3,6]


     Complexity? O(Z^2)
     */
    @tailrec
    def flatMapTailrec(remaining: RList[T], accumulator: RList[S]): RList[S] = {
      if (remaining.isEmpty) accumulator.reverse
      else flatMapTailrec(remaining.tail, f(remaining.head).reverse ++ accumulator)
    }

    /*
      [1,2,3].flatMap(x => [x, 2 * x]) = betterFlatMap([1,2,3], [])
      = betterFlatMap([2,3], [[2,1]])
      = betterFlatMap([3], [[4,2], [2,1]])
      = betterFlatMap([], [[6,3], [4,2], [2,1]])
      = concatenateAll([[6,3], [4,2], [2,1]], [], [])
      = concatenateAll([[4,2], [2,1]], [6,3], [])
      = concatenateAll([[4,2], [2,1]], [3], [6])
      = concatenateAll([[4,2], [2,1]], [], [3,6])
      = concatenateAll([[2,1]], [4,2], [3,6])
      = concatenateAll([[2,1]], [2], [4,3,6])
      = concatenateAll([[2,1]], [], [2,4,3,6])
      = concatenateAll([], [2,1], [2,4,3,6])
      = concatenateAll([], [1], [2,2,4,3,6])
      = concatenateAll([], [], [1,2,2,4,3,6])
      = [1,2,2,4,3,6]

      Complexity: O(N + Z)
     */
    @tailrec
    def betterFlatMap(remaining: RList[T], accumulator: RList[RList[S]]): RList[S] = {
      if (remaining.isEmpty) concatenateAll(accumulator, RNil, RNil)
      else betterFlatMap(remaining.tail, f(remaining.head).reverse :: accumulator)
    }

    /*
      Complexity: O(Z)
     */
    @tailrec
    def concatenateAll(elements: RList[RList[S]], currentList: RList[S], accumulator: RList[S]): RList[S] = {
      if (currentList.isEmpty && elements.isEmpty) accumulator
      else if (currentList.isEmpty) concatenateAll(elements.tail, elements.head, accumulator)
      else concatenateAll(elements, currentList.tail, currentList.head :: accumulator)
    }

    betterFlatMap(this, RNil)
  }

  override def filter(predicate: T => Boolean): RList[T] = {
    /*
      [1,2,3,4,5].filter(x => x % 2 == 0) = filterTailrec([1,2,3,4,5], [])
      = filterTailrec([2,3,4,5], [])
      = filterTailrec([3,4,5], [2])
      = filterTailrec([4,5], [2])
      = filterTailrec([5], [4,2])
      = filterTailrec([], [4,2])
      = [2,4]

      Complexity: O(N)
     */
    @tailrec
    def filterTailrec(remaining: RList[T], accumulator: RList[T]): RList[T] = {
      if (remaining.isEmpty) accumulator.reverse
      else if (predicate(remaining.head)) filterTailrec(remaining.tail, remaining.head :: accumulator)
      else filterTailrec(remaining.tail, accumulator)
    }

    filterTailrec(this, RNil)
  }

  /**
    * Medium difficulty problems
    */
  // run-length encoding
  override def rle: RList[(T, Int)] = {
    /*
      [1,1,1,2,2,3,4,4,4,5].rle = rleTailrec([1,1,2,2,3,4,4,4,5], (1, 1), []) =
      = rlet([1,2,2,3,4,4,4,5], (1,2), [])
      = rlet([2,2,3,4,4,4,5], (1,3), [])
      = rlet([2,3,4,4,4,5], (2,1), [(1,3)])
      = rlet([3,4,4,4,5], (2,2), [(1,3)])
      = rlet([4,4,4,5], (3,1), [(2,2), (1,3)]
      = ...
      = [(5,1), (4,3), (3,1), (2,2), (1,3)]

      Complexity: O(N)
     */
    @tailrec
    def rleTailrec(remaining: RList[T], currentTuple: (T, Int), accumulator: RList[(T, Int)]): RList[(T, Int)] = {
      if (remaining.isEmpty && currentTuple._2 == 0) accumulator
      else if (remaining.isEmpty) currentTuple :: accumulator
      else if (remaining.head == currentTuple._1) rleTailrec(remaining.tail, currentTuple.copy(_2 = currentTuple._2 + 1), accumulator)
      else rleTailrec(remaining.tail, (remaining.head, 1), currentTuple :: accumulator)
    }

    rleTailrec(this.tail, (this.head, 1), RNil).reverse
  }

  // duplicate each element a number of times in a row
  override def duplicateEach(k: Int): RList[T] = {
    /*
      [1,2].duplicateEach(3) = duplicateTailrec([2], 1, 0, [])
      = duplicateTailrec([2], 1, 1, [1])
      = duplicateTailrec([2], 1, 2, [1,1])
      = duplicateTailrec([2], 1, 3, [1,1,1])
      = duplicateTailrec([], 2, 0, [1,1,1])
      = duplicateTailrec([], 2, 1, [2,1,1,1])
      = duplicateTailrec([], 2, 2, [2,2,1,1,1])
      = duplicateTailrec([], 2, 3, [2,2,2,1,1,1])

      Complexity: O(N * K)
     */
    @tailrec
    def duplicateTailrec(remaining: RList[T], currentElement: T, nDuplications: Int, accumulator: RList[T]): RList[T] = {
      if (remaining.isEmpty && nDuplications == k) accumulator.reverse
      else if (remaining.isEmpty) duplicateTailrec(remaining, currentElement, nDuplications + 1, currentElement :: accumulator)
      else if (nDuplications == k) duplicateTailrec(remaining.tail, remaining.head, 0, accumulator)
      else duplicateTailrec(remaining, currentElement, nDuplications + 1, currentElement :: accumulator)
    }

    duplicateTailrec(this.tail, this.head, 0, RNil)
  }

  // rotate by a number of positions to the left
  override def rotate(k: Int): RList[T] = {
    /*
      [1,2,3].rotate(3) == [1,2,3]
      [1,2,3].rotate(6) == [1,2,3]
      [1,2,3].rotate(4) == [1,2,3].rotate(1)

      [1,2,3].rotate(1) = rotateTailrec([1,2,3], 1, [])
      = rotateTailrec([2,3], 0, [1])
      = [2,3,1]

      [1,2,3].rotate(3) = rotateTailrec([1,2,3], 3, [])
      = rotateTailrec([2,3], 2, [1])
      = rotateTailrec([3], 1, [2,1])
      = rotateTailrec([], 0, [3,2,1])
      = [1,2,3]

      [1,2,3].rotate(4) = rotateTailrec([1,2,3], 4, [])
      = rotateTailrec([2,3], 3, [1])
      = rotateTailrec([3], 2, [2,1])
      = rotateTailrec([], 1, [3,2,1])
      = rotateTailrec([1,2,3], 1, [])
      = [2,3,1]

      Complexity: O(max(N, K))
     */
    @tailrec
    def rotateTailrec(remaining: RList[T], rotationsLeft: Int, buffer: RList[T]): RList[T] = {
      if (remaining.isEmpty && rotationsLeft == 0) this
      else if (remaining.isEmpty) rotateTailrec(this, rotationsLeft, RNil)
      else if (rotationsLeft == 0) remaining ++ buffer.reverse
      else rotateTailrec(remaining.tail, rotationsLeft - 1, remaining.head :: buffer)
    }

    rotateTailrec(this, k, RNil)
  }

  // random samples
  override def sample(k: Int): RList[T] = {
    val random = new Random(System.currentTimeMillis())
    val maxIndex = this.length

    /*
      [1,2,3,4,5].sample(3) = sampleTailrec(3, [])
      = sampleTailrec(2, [2])
      = sampleTailrec(1, [4,2])
      = sampleTailrec(0, [4,4,2])
      = [4,4,2]

      Complexity: O(N * K)
     */
    @tailrec
    def sampleTailrec(nRemaining: Int, accumulator: RList[T]): RList[T] = {
      if (nRemaining == 0) accumulator
      else {
        val index = random.nextInt(maxIndex)
        val newNumber = this(index)
        sampleTailrec(nRemaining - 1, newNumber :: accumulator)
      }
    }

    /*
      Complexity: O(N * K)
     */
    def sampleElegant: RList[T] =
      RList.from((1 to k).map(_ => random.nextInt(maxIndex)).map(index => this(index)))

    if (k < 0) RNil
    else sampleElegant
  }

  /**
    * Hard problems
    */
  override def insertionSort[S >: T](ordering: Ordering[S]): RList[S] = {
    /*
      insertSorted(4, [], [1,2,3,5]) =
      insertSorted(4, [1], [2,3,5]) =
      insertSorted(4, [2,1], [3,5]) =
      insertSorted(4, [3,2,1], [5]) =
      [3,2,1].reverse + (4 :: [5]) =
      [1,2,3,4,5]

      Complexity: O(N)
     */
    @tailrec
    def insertSorted(element: T, before: RList[S], after: RList[S]): RList[S] = {
      if (after.isEmpty || ordering.lteq(element, after.head)) before.reverse ++ (element :: after)
      else insertSorted(element, after.head :: before, after.tail)
    }

    /*
      [3,1,4,2,5].sorted = insertSortTailrec([3,1,4,2,5], []) =
        = insertSortTailrec([1,4,2,5], [3])
        = insertSortTailrec([4,2,5], [1,3])
        = insertSortTailrec([2,5], [1,3,4])
        = insertSortTailrec([5], [1,2,3,4])
        = insertSortTailrec([], [1,2,3,4,5])
        = [1,2,3,4,5]

        Complexity: O(N^2)
     */
    @tailrec
    def insertSortTailrec(remaining: RList[T], acc: RList[S]): RList[S] = {
      if (remaining.isEmpty) acc
      else insertSortTailrec(remaining.tail, insertSorted(remaining.head, RNil, acc))
    }

    insertSortTailrec(this, RNil)
  }

  override def mergeSort[S >: T](ordering: Ordering[S]): RList[S] = {
    /*
      merge([1,3], [2,4,5,6,7], []) =
      merge([3], [2,4,5,6,7], [1]) =
      merge([3], [4,5,6,7], [2,1]) =
      merge([], [4,5,6,7], [3,2,1]) =
      [1,2,3] ++ [4,5,6,7] =
      [1,2,3,4,5,6,7]
     */
    @tailrec
    def merge(listA: RList[S], listB: RList[S], accumulator: RList[S]): RList[S] = {
      if (listA.isEmpty) accumulator.reverse ++ listB
      else if (listB.isEmpty) accumulator.reverse ++ listA
      else if (ordering.lteq(listA.head, listB.head)) merge(listA.tail, listB, listA.head :: accumulator)
      else merge(listA, listB.tail, listB.head :: accumulator)
    }

    /*
      [3,1,2,5,4] => [[3],[1],[2],[5],[4]]

      mst([[3],[1],[2],[5],[4]], []) =
      = mst([[2],[5],[4]], [[1,3]])
      = mst([[4]], [[2,5], [1,3]])
      = mst([], [[4], [2,5], [1,3]]) =
      = mst([[4], [2,5], [1,3]], [])
      = mst([[1,3]], [[2,4,5]])
      = mst([], [[1,3], [2,4,5]])
      = mst([[1,3], [2,4,5]], [])
      = mst([], [[1,2,3,4,5]])
      = [1,2,3,4,5]

      Complexity: O(n * log(n))
      complexity(n) = 2 * complexity(n/2) + n
     */
    @tailrec
    def mergeSortTailrec(smallLists: RList[RList[S]], bigLists: RList[RList[S]]): RList[S] = {
      if (smallLists.isEmpty) {
        if (bigLists.isEmpty) RNil
        else if (bigLists.tail.isEmpty) bigLists.head
        else mergeSortTailrec(bigLists, RNil)
      } else if (smallLists.tail.isEmpty) {
        if (bigLists.isEmpty) smallLists.head
        else mergeSortTailrec(smallLists.head :: bigLists, RNil)
      } else {
        val first = smallLists.head
        val second = smallLists.tail.head
        val merged = merge(first, second, RNil)
        mergeSortTailrec(smallLists.tail.tail, merged :: bigLists)
      }
    }

    mergeSortTailrec(this.map(x => x :: RNil), RNil)
  }

  override def quickSort[S >: T](ordering: Ordering[S]): RList[S] = {
    /*
      partition([1,2,5,4], 3, [], []) =
      partition([2,5,4], 3, [1], []) =
      partition([5,4], 3, [2,1], []) =
      partition([4], 3, [2,1], [5]) =
      partition([], 3, [2,1], [4,5])
      = ([2,1], [4,5])
     */
    @tailrec
    def partition(list: RList[T], pivot: T, smaller: RList[T], larger: RList[T]): (RList[T], RList[T]) = {
      if (list.isEmpty) (smaller, larger)
      else if (ordering.lteq(list.head, pivot)) partition(list.tail, pivot, list.head :: smaller, larger)
      else partition(list.tail, pivot, smaller, list.head :: larger)
    }

    /*
      [3,1,2,5,4].quickSort

      partition([1,2,5,4], 3, [], []) -> ([1,2], [5,4])
      partition([2], 1, [], []) -> ([], [2])
      partition([4], 5, [], []) -> ([4], [])

      quickSortTailrec([[3,1,2,5,4]], []) =
      quickSortTailrec([[1,2], [3], [5,4]], []) =
      quickSortTailrec([[], [1], [2], [3], [5,4]], []) =
      quickSortTailrec([[1], [2], [3], [5,4]], []) =
      quickSortTailrec([[2], [3], [5,4]], [[1]]) =
      quickSortTailrec([[3], [5,4]], [[2], [1]]) =
      quickSortTailrec([[5,4]], [[3],[2],[1]]) =
      quickSortTailrec([[4], [5], []], [[3],[2],[1]]) =
        ...
      quickSortTailrec([], [[5], [4], [3], [2], [1]]) =
      [1,2,3,4,5]

      Complexity: O(N^2) in the worst case (when the list is sorted)
      on average O(N * log(N))
     */
    @tailrec
    def quickSortTailrec(remainingLists: RList[RList[T]], accumulator: RList[RList[T]]): RList[T] = {
      if (remainingLists.isEmpty) accumulator.flatMap(smallList => smallList).reverse
      else if (remainingLists.head.isEmpty) quickSortTailrec(remainingLists.tail, accumulator)
      else if (remainingLists.head.tail.isEmpty) quickSortTailrec(remainingLists.tail, remainingLists.head :: accumulator)
      else {
        val list = remainingLists.head
        val pivot = list.head
        val listToSplit = list.tail
        val (smaller, larger) = partition(listToSplit, pivot, RNil, RNil)
        quickSortTailrec(smaller :: (pivot :: RNil) :: larger :: remainingLists.tail, accumulator)
      }
    }

    quickSortTailrec(this :: RNil, RNil)
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
