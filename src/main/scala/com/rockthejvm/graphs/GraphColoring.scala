package com.rockthejvm.graphs

import scala.annotation.tailrec

object GraphColoring {
  import GraphProblems._

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
    val socialNetwork: Graph[String] = Map(
      "Alice" -> Set("Bob", "Charlie", "David"),
      "Bob" -> Set(),
      "Charlie" -> Set("David"),
      "David" -> Set("Bob", "Mary"),
      "Mary" -> Set("Bob", "Charlie")
    )
    println(color(socialNetwork))
  }

  def main(args: Array[String]): Unit = {
    testColor()
  }
}
