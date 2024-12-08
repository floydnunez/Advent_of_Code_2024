package day04

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.boundary
import scala.util.boundary.break
import scala.util.matching.Regex

@main def main(): Unit =
  val fileName = "inputs/day04/input.txt"
  val fileContents = Source.fromFile(fileName).getLines().toList
  println("File contents:")

  var total = 0
  var total_2 = 0
  var bigLine = ""
  var largo = 0
  var forwardDiagonalStr = ListBuffer[String]()
  var backwardDiagonalStr = ListBuffer[String]()
  val height = fileContents.length
  var forward = 0
  var backward = height
  fileContents.foreach( line => {
    largo = line.length
    bigLine += line + ";"
    val fline = ";".repeat(forward) + line
    forwardDiagonalStr += fline
    forward += 1
    val bline = ";".repeat(backward) + line
    backwardDiagonalStr += bline
    backward -= 1
  })
  val transposedString = rotateStrings(fileContents)
  val transposedFDiagString = rotateStrings(forwardDiagonalStr.toList)
  val transposedBDiagString = rotateStrings(backwardDiagonalStr.toList)

  var bigTransposedString = ""
  transposedString.foreach( line => {
    bigTransposedString += line + ";"
  })
  var bigTransposedFdiagString = ""
  transposedFDiagString.foreach( line => {
    bigTransposedFdiagString += line + ";"
  })
  var bigTransposedBDiagString = ""
  transposedBDiagString.foreach( line => {
    bigTransposedBDiagString += line + ";"
  })

  println(s"bigline: ${bigLine}")
  {
    val matches: Int = findAll(bigLine, "horizontal",0)
    total += matches
  }
  {
    val matches: Int = findAll(bigTransposedString, "vertical",0)
    total += matches
  }
  {
    val matches: Int = findAll(bigTransposedFdiagString, "forward diagonal",0)
    total += matches
  }
  {
    val matches: Int = findAll(bigTransposedBDiagString, "backward diagonal",0)
    total += matches
  }

  println(s"total part 1 = ${total}") // 2571
//Part 2
  {
    total_2 += countXmas(total_2, bigLine, largo, height, "M", "M", "A", "S", "S")
    total_2 += countXmas(total_2, bigLine, largo, height, "M", "S", "A", "M", "S")
    total_2 += countXmas(total_2, bigLine, largo, height, "S", "S", "A", "M", "M")
    total_2 += countXmas(total_2, bigLine, largo, height, "S", "M", "A", "S", "M")
  }
  println(s"total part 2 = ${total_2}") // 1992

def countXmas(total_2: Int, bigLine: String, largo: Int, height: Int, t1: String, t2: String, c: String, b1: String, b2: String): Int = {
  val xmas = generateRegex(largo, t1, t2, c, b1, b2)
  return findAll(bigLine, "X-MAS " + t1+t2+c+b1+b2, height, xmas)
}

def generateRegex(largo: Int, t1: String, t2: String, c: String, b1: String, b2: String): scala.util.matching.Regex = {
  val str = "(?=" + t1 + "[XMAS]{1}" + t2 + "[XMAS;]{" + (largo-1) + "}"+ c +"[XMAS;]{" + (largo - 1) + "}"+b1+"[XMAS]{1}" + b2 + ")"
  println(s"regex: ${str}")
  return str.r
}

def findAll(bigLine: String, name: String, largo: Int, pattern: scala.util.matching.Regex = "(?=XMAS|SAMX)".r) = {
  val matches = pattern.findAllMatchIn(bigLine).toList
  if (largo > 0) {
    matches.foreach { m =>
      val start = m.start
      val line = start / largo
      val pos = start % largo
      println(s"found at: ${line}, ${pos}")
    }
  }
  val horizontalMatches = matches.length
  println(s"${name}: ${horizontalMatches}")
  horizontalMatches
}


def rotateStrings(lines: List[String]): List[String] = {
  val maxLength = lines.map(_.length).max // Find the longest string
  val paddedLines = lines.map(_.padTo(maxLength, ' ')) // Pad shorter lines with spaces
  paddedLines.transpose.map(_.mkString) // Transpose and join characters
}