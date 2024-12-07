package day01

import scala.collection.mutable.ListBuffer
import scala.io.Source

@main def main(): Unit =
  val fileName = "inputs/day01/input.txt"
  val fileContents = Source.fromFile(fileName).getLines().toList
  println("File contents:")

  val left = ListBuffer[Int]()
  val right = ListBuffer[Int]()

  fileContents.foreach{
    line =>
      val parts = line.split("   ")
      left +=  parts(0).toInt
      right += parts(1).toInt
  }

  val sortedLeft = left.toList.sorted
  val sortedRight = right.toList.sorted

  val diffs = ListBuffer[Int]()
  sortedLeft.zip(sortedRight).foreach { case (l, r) =>
    diffs += Math.abs(l - r)
  }

  println(s"diffs: ${diffs.toList.sum}") //2057374 is the answer
  //part 2

  var total_part_2 = 0
  val rList = right.toList
  left.toList.foreach( l => {
    val instances = countInstances(l, rList)
    total_part_2 += instances * l
  })
  println(s"Part 2:  ${total_part_2}") // 23177084

def countInstances(value: Int, list: List[Int]): Int = {
  val result = list.count(_ == value)
  println(s"count for ${value} in ${list} = ${result}")
  return result
}