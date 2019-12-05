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
  def outDegree[T](graph: Graph[T], node: T): Int = {
    if (graph.contains(node)) graph(node).size
    else 0
  }

  // number of nodes connected to `node`
  def inDegree[T](graph: Graph[T], node: T): Int = {
    graph.values.count(_.contains(node))
  }

  def testDegrees(): Unit = {
    println(outDegree(socialNetwork, "Alice")) // 3
    println(inDegree(socialNetwork, "David")) // 2
  }

  /**
    * Medium difficulty problems
    */

  def isPath[T](graph: Graph[T], start: T, end: T): Boolean = {
    /*
      Alice -> Mary

      ipt([Alice], []) =
      ipt([Bob, Charlie, David], [Alice]) =
      ipt([Charlie, David], [Bob, Alice]) =
      ipt([David, David], [Charlie, Bob, Alice]) =
      ipt([David, Bob, Mary], [David, Charlie, Bob, Alice]) =
      ipt([Bob, Mary], [David, Charlie, Bob, Alice]) =
      ipt([Mary], [David, Charlie, Bob, Alice]) =
      = true

      N nodes, E edges
      Complexity: O(E)
     */
    @tailrec
    def isPathTailrec(remaining: List[T], consideredNodes: Set[T]): Boolean = {
      if (remaining.isEmpty) false
      else {
        val node = remaining.head
        if (node == end) true
        else if (consideredNodes.contains(node)) isPathTailrec(remaining.tail, consideredNodes)
        else isPathTailrec(remaining.tail ++ graph(node), consideredNodes + node)
      }
    }

    isPathTailrec(List(start), Set())
  }

  def testPaths(): Unit = {
    println(isPath(socialNetwork, "Alice", "Mary")) // true
    println(isPath(socialNetwork, "Bob", "Mary")) // false
  }

  def findPath[T](graph: Graph[T], start: T, end: T): List[T] = {
    /*
      Charlie -> Mary

      fpt([(Charlie, [Charlie])], []) =
        neighbors = [David]
        tuples = [(David, [David, Charlie])]

      fpt([(David, [David, Charlie])], [Charlie]) =
        neighbors = [Bob, Mary]
        tuples = [(Bob, [Bob, David, Charlie], (Mary [Mary, David, Charlie])]

      fpt([(Bob, [Bob, David, Charlie]), (Mary [Mary, David, Charlie])], [David, Charlie]) =
        neighbors = []
        tuples = []

      fpt([(Mary, [Mary, David, Charlie])], [David, Charlie, Bob]) =

      [Charlie, David, Mary]
     */
    @tailrec
    def findPathTailrec(remaining: List[(T, List[T])], consideredNodes: Set[T]): List[T] = {
      if (remaining.isEmpty) List()
      else {
        val (node, currentPath) = remaining.head
        if (node == end) currentPath.reverse
        else if (consideredNodes.contains(node)) findPathTailrec(remaining.tail, consideredNodes)
        else {
          val neighbors = graph(node)
          val tuples = neighbors.map(n => (n, n :: currentPath))
          findPathTailrec(remaining.tail ++ tuples, consideredNodes + node)
        }
      }
    }

    findPathTailrec(graph(start).map(n => (n, n :: List(start))).toList, Set(start))
  }

  def findCycle[T](graph: Graph[T], node: T): List[T] = findPath(graph, node, node)

  def testFindPath(): Unit = {
    println(findPath(socialNetwork, "Charlie", "Mary"))
    println(findPath(socialNetwork, "Alice", "Mary"))
    println(findPath(socialNetwork, "Bob", "Mary"))
  }

  def testCycles(): Unit = {
    println(findCycle(socialNetwork, "Alice")) // List
  }

  def makeUndirected[T](graph: Graph[T]): Graph[T] = {
    def addEdge(graph: Graph[T], from: T, to: T): Graph[T] = {
      if (!graph.contains(from)) graph + (from -> Set(to))
      else {
        val neighbors = graph(from)
        graph + (from -> (neighbors + to))
      }
    }

    @tailrec
    def addOpposingEdges(remainingNodes: Set[T], accumulator: Graph[T]): Graph[T] = {
      if (remainingNodes.isEmpty) accumulator
      else {
        val node = remainingNodes.head
        val neighbors = graph(node)
        val newGraph = neighbors.foldLeft(accumulator)((intermediateGraph, neighbor) => addEdge(intermediateGraph, neighbor, node))
        addOpposingEdges(remainingNodes.tail, newGraph)
      }
    }

    addOpposingEdges(graph.keySet, graph)
  }

  def testUndirected(): Unit = {
    val undirectedNetwork = makeUndirected(socialNetwork)
    println(undirectedNetwork("Bob"))
    println(undirectedNetwork("Alice"))
    println(undirectedNetwork("David"))
  }

  /**
    * Hard problems
    */


  def color[T](graph: Graph[T]): Map[T, Int] = {
    val undirected = makeUndirected(graph)

    /*
        Alice -> [Bob, Charlie, David]
        Bob -> [Alice, David, Mary]
        Charlie -> [David, Alice, Mary]
        David -> [Bob, Mary, Alice, Charlie]
        Mary -> [Bob, Charlie, David]


        [David, Alice, Bob, Charlie, Mary]

        colorTailrec([David, Alice, Bob, Charlie, Mary], 0, {}) =
        colorTailrec([Alice, Bob, Charlie, Mary], 1, { David -> 0 }) =
        colorTailrec([Bob, Charlie, Mary], 2, { David -> 0, Alice -> 1, Mary -> 1}) =
        colorTailrec([Charlie, Mary], 3, { David -> 0, Alice -> 1, Mary -> 1, Bob -> 2, Charlie -> 2 })
        colorTailrec([Mary], 3, { David -> 0, Alice -> 1, Mary -> 1, Bob -> 2, Charlie -> 2 })
        colorTailrec([], 3, { David -> 0, Alice -> 1, Mary -> 1, Bob -> 2, Charlie -> 2 })
        { David -> 0, Alice -> 1, Mary -> 1, Bob -> 2, Charlie -> 2 }

     */

    @tailrec
    def colorTailrec(remainingNodes: List[T], currentColor: Int, colorings: Map[T, Int]): Map[T, Int] = {
      if (remainingNodes.isEmpty) colorings
      else {
        val node = remainingNodes.head
        if (colorings.contains(node)) colorTailrec(remainingNodes.tail, currentColor, colorings)
        else {
          val uncoloredNodes = remainingNodes.tail.foldLeft[Set[T]](Set(node)) { (nodesToBeColored, n) =>
            val allNeighbors = nodesToBeColored.flatMap(nodeToBeColored => undirected(nodeToBeColored))
            if (colorings.contains(n) || allNeighbors.contains(n)) nodesToBeColored
            else nodesToBeColored + n
          }

          val newColorings = uncoloredNodes.map((_, currentColor)).toMap
          colorTailrec(remainingNodes.tail, currentColor + 1, colorings ++ newColorings)
        }
      }
    }


    val nodesOrdered = undirected.keySet.toList.sortWith((a, b) => outDegree(undirected, a) > outDegree(undirected, b))
    colorTailrec(nodesOrdered, 0, Map())
  }

  def testColor(): Unit = {
    println(color(socialNetwork))
  }

  testColor()

}
