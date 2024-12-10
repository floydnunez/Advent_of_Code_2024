package day10

import day07.Oper.{add, mul}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.util.boundary
import scala.util.boundary.break
import scala.util.matching.Regex

case class Square(x: Int, y: Int, height: Int, steps: Int)

@main def main(): Unit =
  val fileName = "inputs/day10/input.txt"
  val fileContents = Source.fromFile(fileName).getLines().toArray

  printArray(fileContents)

  val height = fileContents.length
  val width = fileContents(0).length

  val bigLine = fileContents.mkString("")

  val (zeroes, map) = getAllZeros(bigLine, width, height)
  println(zeroes)
  var total_1 = 0
  var total_2 = 0
  zeroes.foreach { zero =>
    val mutableMapCopy = mutable.Map(map.toSeq: _*)
    val hiketrails = djikstra(Some(zero), mutableMapCopy, 0)
    val subTotal = mutableMapCopy.count { case (_, value) => value.steps == 9 }
    println(s"total for ${zero} = ${subTotal} hiketrails: ${hiketrails}")
    total_1 += subTotal
    total_2 += hiketrails
  }
  println(s"Day 10 part 1: ${total_1}") // 811
  println(s"Day 10 part 2: ${total_2}") // 1794

def djikstra(optionFrom: Option[Square],map: mutable.Map[(Int, Int), Square], steps: Int): Int =
  optionFrom match {
    case Some(from) if from.height == steps =>
      map((from.x, from.y)) = from.copy(steps = steps)
      if (steps == 9) {
        return 1
      }
      val neighbors = List(
        (from.x, from.y - 1),
        (from.x + 1, from.y),
        (from.x, from.y + 1),
        (from.x - 1, from.y)
      )
      var total = 0
      neighbors.foreach { case (nx, ny) =>
        map.get((nx, ny)).foreach{ neighbor =>
         total += djikstra(Some(neighbor), map, steps + 1)
        }
      }
      return total
    case _ => 0
  }

def printArray(arr: Array[String]): Unit =
  arr.foreach(println)

def getAllZeros(strings: String, width: Int, height: Int): (List[Square], Map[(Int, Int), Square]) =
  val result_zeros = ListBuffer[Square]()
  var map = Map[(Int, Int), Square]()

  strings.zipWithIndex.foreach{ case(letter, index) =>
    val indexX = index % width
    val indexY = index / width
    val value = letter.asDigit
    val square = Square(indexX, indexY, value, -1)
    if (value == 0) {
      result_zeros += square
    }
    map = map.updated((indexX, indexY), square)
  }
  return (result_zeros.toList, map)