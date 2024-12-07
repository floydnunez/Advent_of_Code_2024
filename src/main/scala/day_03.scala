package day03

import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.boundary
import scala.util.boundary.break


@main def main(): Unit =
  val fileName = "inputs/day03/input.txt"
  val fileContents = Source.fromFile(fileName).getLines().toList
  println("File contents:")

  var total = 0
  var total_2 = 0
  var bigLine = ""
  fileContents.foreach( line => {
    val pattern = "mul\\((\\d+),(\\d+)\\)".r
    val matches = pattern.findAllMatchIn(line).toList
    matches.foreach( mul =>
      val arg1 = mul.group(1).toInt
      val arg2 = mul.group(2).toInt
      total += arg1 * arg2
    )
    bigLine += line
  })
  val pattern = "mul\\((\\d+),(\\d+)\\)|do\\(\\)|don\\'t\\(\\)".r
  val matches = pattern.findAllMatchIn(bigLine).toList
  var enabled = true
  matches.foreach(op =>
    val opcode = op.group(0)
    println(s"opcode: ${opcode}")
    if (opcode == "do()") {
      enabled = true
    } else if (opcode == "don't()") {
      enabled = false
    } else {
      if (enabled) {
        val arg1 = op.group(1).toInt
        val arg2 = op.group(2).toInt
        total_2 += arg1 * arg2
      }
    }
  )

  println(s"total part 1 = ${total}") // 178538786
  println(s"total part 2 = ${total_2}") // 102467299

