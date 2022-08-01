package com.rockthejvm.graphs

import scala.annotation.tailrec

object GraphProblems extends App {

  type Graph[T] = Map[T, Set[T]]

  val socialNetwork: Graph[String] = Map(
    "Alice" -> Set("Bob", "Charlie", "David"),
    "Bob" -> Set(),
    "Charlie" -> Set("David"),
    "David" -> Set("Bob", "Mary"),
    "Mary" -> Set("Bob", "Charlie")
  )

  /**
    * Easy problems
    */

  // number of nodes this node `node` is associated (adjacent) to
  def outDegree[T](graph: Graph[T], node: T): Int = ???

  // number of nodes connected to `node`
  def inDegree[T](graph: Graph[T], node: T): Int = ???

  def testDegrees(): Unit = {
    println(outDegree(socialNetwork, "Alice")) // 3
    println(inDegree(socialNetwork, "David")) // 2
  }

  /**
    * Medium difficulty problems
    */

  def isPath[T](graph: Graph[T], start: T, end: T): Boolean = ???

  def testPaths(): Unit = {
    println(isPath(socialNetwork, "Alice", "Mary")) // true
    println(isPath(socialNetwork, "Bob", "Mary")) // false
  }

  def findPath[T](graph: Graph[T], start: T, end: T): List[T] = ???

  def findCycle[T](graph: Graph[T], node: T): List[T] = ???

  def testFindPath(): Unit = {
    println(findPath(socialNetwork, "Charlie", "Mary"))
    println(findPath(socialNetwork, "Alice", "Mary"))
    println(findPath(socialNetwork, "Bob", "Mary"))
  }

  def testCycles(): Unit = {
    println(findCycle(socialNetwork, "Alice")) // List
  }

  def makeUndirected[T](graph: Graph[T]): Graph[T] = ???

  def testUndirected(): Unit = {
    val undirectedNetwork = makeUndirected(socialNetwork)
    println(undirectedNetwork("Bob"))
    println(undirectedNetwork("Alice"))
    println(undirectedNetwork("David"))
  }

}
