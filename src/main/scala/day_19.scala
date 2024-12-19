package day19

import day17.Expected.{Exact, No, Substring}

import java.text.NumberFormat
import java.util.Locale
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.math.{E, pow}
import scala.util.boundary.break
import scala.util.{Random, boundary}

var debug = true
def printDebug(str: String): Unit =
  if (debug) {
    println(str)
  }

def generateRegex(towels: List[String], pattern: String) = {
  val setPossibleTowels = towels.filter(pattern.contains(_))
  printDebug(s"possible towels for ${pattern} = ${setPossibleTowels}")
  val allRegexes = ListBuffer[String]()
  var regex = "^("
  regex += setPossibleTowels.mkString("|")
  regex += ")*$"
  (setPossibleTowels, regex)
}
@main def main(): Unit =
  val startTime = System.nanoTime()
  val fileName = "inputs/day19/input.txt"
  if (fileName.contains("input.txt") || fileName.contains("challenge")) {
    debug = false
  }

  val fileContents = Source.fromFile(fileName).getLines().toArray

  val towels = fileContents(0).split(",").map(_.trim).toList
  printDebug(s"${towels}")

  val patterns = fileContents.slice(2, fileContents.length).toList
  printDebug(s"${patterns}")

  var countPossible = 0
  var countAllPossible = 0L
  patterns.foreach{ pattern =>
    val generateRegexResult: (List[String], String) = generateRegex(towels, pattern)
    val setPossibleTowels: List[String] = generateRegexResult._1
    var regex: String = generateRegexResult._2
    println(s"trying to match ${pattern} with ${regex}")
    if (pattern.matches(regex)) {
      countPossible += 1
      val allMatches = segmentString(pattern, setPossibleTowels)
      println(s"all matches: ${allMatches}")
      countAllPossible += allMatches
      println(s"   for ${pattern} all possible ways: ${allMatches} subtotal ${countAllPossible}")
    }
  }

  println(s"Day 19 Part 1: ${countPossible}")
  println(s"Day 19 Part 2: ${countAllPossible}")

def segmentString(input: String, patterns: List[String]): Long = {
  val memo = scala.collection.mutable.Map[String, Long]()
  def helper(input: String): Long = {
    if (input.isEmpty) return 1L
    if (memo.contains(input)) return memo(input)

    var result = 0L
    patterns.foreach { pattern =>
      if (input.startsWith(pattern)) {
        val substr = input.substring(pattern.length)
        result += helper(substr)
      }
    }
    memo(input) = result
    result
  }
  helper(input)
}
