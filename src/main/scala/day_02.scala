package day02

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.boundary
import scala.util.boundary.break


@main def main(): Unit =
  val fileName = "inputs/day02/input.txt"
  val fileContents = Source.fromFile(fileName).getLines().toList
  println("File contents:")

  var total = 0
  var total_2 = 0
  fileContents.foreach( line => {
    val lineSplit = line.split(" ")
    val nums = lineSplit.map(_.toInt)
    val r_nums = nums.reverse
    println(s"${nums.mkString(" ")}  /  ${r_nums.mkString(" ")}")
    if (isValid(nums) || isValid(r_nums) ) {
      total += 1
    }
    boundary {
      for (i <- nums.indices) {
        val copyNums_i = nums.patch(i, Nil, 1)
        val copyRNums_i = r_nums.patch(i, Nil, 1)
        if (isValid(copyNums_i) || isValid(copyRNums_i)) {
          total_2 += 1
          break()
        }
      }
    }
  })
  println(s"total part 1 = ${total}") // 224
  println(s"total part 2 = ${total_2}") // 293

def isValid(ints: Array[Int]): Boolean =
  ints.sliding(2).foreach {
    case Array(prev, curr) =>
      println(s"Previous: $prev, Current: $curr")
      if (curr - prev < 1 || curr - prev > 3) {
        return false
      }
  }
  return true