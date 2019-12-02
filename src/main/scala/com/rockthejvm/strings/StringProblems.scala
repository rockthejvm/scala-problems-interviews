package com.rockthejvm.strings

import scala.annotation.tailrec

object StringProblems extends App {

  def countCharacters(s: String): Map[Char, Int] = {
    /*
      cct("Scala", []) = cct("cala", [S -> 1])
      = cct("ala", [c -> 1, S -> 1])
      = cct("la", [a -> 1, c -> 1, S -> 1])
      = cct("a", [l -> 1, a -> 1, c -> 1, S -> 1])
      = cct("", [a -> 2, l -> 1, c -> 1, S -> 1])
      = [a -> 2, l -> 1, c -> 1, S -> 1]
     */
    @tailrec
    def countCharactersTailrec(remaining: String, acc: Map[Char, Int]): Map[Char, Int] =
      if (remaining.isEmpty) acc
      else if (acc.contains(remaining.head)) {
        val currentChar = remaining.head
        val currentOccurrences = acc(currentChar)
        countCharactersTailrec(remaining.tail, acc + (currentChar -> (currentOccurrences + 1)))
      } else countCharactersTailrec(remaining.tail, acc + (remaining.head -> 1))

    countCharactersTailrec(s, Map())
  }

  def testCountCharacters() = {
    println(countCharacters("Scala"))
    println(countCharacters("I love Scala and functional programming because it's awesome!"))
  }


  def checkAnagrams(sa: String, sb: String): Boolean = countCharacters(sa) == countCharacters(sb)
  def checkAnagrams2(sa: String, sb: String): Boolean = sa.sorted == sb.sorted

  def testCheckAnagrams() = {
    println(checkAnagrams("desserts", "stressed"))
    println(checkAnagrams("Scala", "Haskell"))
    println(checkAnagrams2("desserts", "stressed"))
    println(checkAnagrams2("Scala", "Haskell"))
  }


  def justify(text: String, width: Int): String = {
    def createSpaces(n: Int): String = (1 to n).map(_ => " ").mkString("") // creates n spaces

    @tailrec
    def pack(words: List[String], currentRow: List[String], currentCharCount: Int, result: List[List[String]]): List[List[String]] = {
      if (words.isEmpty && currentRow.isEmpty) {
        // nothing else to add
        result
      } else if (words.isEmpty) {
        // add the last row
        result :+ currentRow
      } else if (currentRow.isEmpty && words.head.length > width) {
        // split the word into supercalifra-gilistic
        val (partOnThisRow, partOnNextRow) = words.head.splitAt(width - 2) // at width - 1 put a '-'
        pack(partOnNextRow :: words.tail, List(), 0, result :+ List(partOnThisRow + "-"))
      } else if (words.head.length + currentCharCount > width) {
        // fetch a new row
        pack(words, List(), 0, result :+ currentRow)
      } else {
        // put the word into the current row
        pack(words.tail, currentRow :+ words.head, currentCharCount + 1 + words.head.length, result)
      }
    }

    /*
      I, love, Scala
      width = 15

      nSpacesAvailable = 5
      nIntervals = 2
      nSpacesPerInteval = 5/2 = 2
      nExtraSpaces = 1
      regularSpace = "  "
      biggerSpace = "   "
      nWordsWithBiggerIntervals = 2
      wordsWithBiggerIntervals = [I, love]
      firstPart = "I   love"
      secondPart = "Scala"
      "I   love  Scala"

      => I   love  Scala

     */
    def justifyRow(row: List[String]): String = {
      if (row.length == 1) row.head
      else {
        val nSpacesAvailable = width - row.map(_.length).sum
        val nIntervals = row.length - 1
        val nSpacesPerInterval = nSpacesAvailable / nIntervals
        val nExtraSpaces = nSpacesAvailable % nIntervals
        val regularSpace = createSpaces(nSpacesPerInterval)
        val biggerSpace = createSpaces(nSpacesPerInterval + 1)

        if (nExtraSpaces == 0) row.mkString(regularSpace)
        else {
          val nWordsWithBiggerIntervals = nExtraSpaces + 1
          val wordsWithBiggerIntervals = row.take(nWordsWithBiggerIntervals)
          val firstPart = wordsWithBiggerIntervals.mkString(biggerSpace)
          val secondPart = row.drop(nWordsWithBiggerIntervals).mkString(regularSpace)

          firstPart + regularSpace + secondPart
        }

      }
    }

    assert(width > 2)
    // split text into words
    val words = text.split(" ").toList
    // pack the words into rows
    val unjustifiedRows = pack(words, List(), 0, List())
    // justify the rows
    val justifiedRows = unjustifiedRows.init.map(justifyRow) :+ unjustifiedRows.last.mkString(" ")
    // rebuild the justified text
    justifiedRows.mkString("\n")
  }


  println(justify("Scala is the most amazing language you will ever write any code in", 6))
  println(justify("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam vel urna bibendum, pharetra mi quis, imperdiet nibh. Praesent dictum odio lacus, eget commodo sem aliquam rutrum. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec eget rhoncus mauris, quis vehicula mi. Quisque finibus purus non varius dictum. Pellentesque vulputate fringilla egestas. Nunc eleifend ex sed egestas cursus. Praesent molestie nisl in pretium vehicula. Vestibulum efficitur ut risus quis porta. Praesent non sem quam. Donec vitae arcu sapien. Quisque aliquet nibh in metus efficitur ullamcorper. Donec mattis dapibus nisl sed iaculis. Curabitur eu blandit enim. Fusce varius.", 80))
  println(justify("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc vestibulum gravida justo, id luctus nulla dapibus ut. Ut elementum ac metus at vestibulum. Quisque pellentesque id nisi sed efficitur. Maecenas consectetur diam ac orci convallis mollis. Etiam sem purus, accumsan consequat mattis at, gravida at justo. Nullam molestie ex non cursus semper. Cras porta nunc sed tempus finibus. Maecenas pretium nibh est, id scelerisque turpis convallis vel. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Aliquam erat volutpat. Duis sed convallis augue. Integer lacus eros, posuere ut metus at, vestibulum consectetur lorem.\n\nPhasellus vel pulvinar sem, a rutrum lectus. Donec quis nisi nec leo faucibus sodales id non nunc. Fusce gravida diam vitae orci sollicitudin, a efficitur urna fermentum. Vivamus gravida ante sed lectus ornare, eget egestas leo porta. Curabitur sed blandit metus. Ut at augue consequat, suscipit nulla eu, euismod tellus. Mauris eu mi faucibus, elementum ligula sed, placerat lectus. Morbi varius magna eu mauris ultricies, vitae blandit purus pharetra. Etiam finibus non odio in eleifend. Cras viverra, dolor at ullamcorper pharetra, ligula nunc mattis est, ut volutpat odio dui vitae ligula. Nulla vitae tincidunt arcu. Nulla facilisi. Duis ullamcorper eros neque, at tincidunt.", 100))
}
