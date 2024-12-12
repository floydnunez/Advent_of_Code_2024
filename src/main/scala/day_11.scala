package day11

import day07.Oper.{add, mul}

import java.text.NumberFormat
import java.util.Locale
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.util.boundary
import scala.util.boundary.break

val toStringMap = mutable.Map[Long, String]()

val formatter = NumberFormat.getInstance(Locale.GERMANY)


@main def main(): Unit =
  val fileName = "inputs/day11/input.txt"
  val fileContents = Source.fromFile(fileName).getLines().toArray
  var line = fileContents(0)
//  line = "1 1 1 1"
  val numbersMut = mutable.Map[Long, Long]()
  line.split(" ").foreach { elem=> numbersMut(elem.toLong) = 1 }
  var numbers = numbersMut.toMap

  println(line)
  println(numbers)

  var largo = 0L
  for(i <- 0 until 25) {
    numbers = blink(numbers)
    largo = numbers.values.sum
    val formattedNumber = formatter.format(largo)
//    printMap(numbers)
    println(s" ${formattedNumber} ${numbers.size} blink: ${i}")
  }
  println(s"Day 11 part 1: ${largo}") // 202019

  var numbersPart2 = numbersMut.toMap

  for(i <- 0 until 75) {
    numbersPart2 = blink(numbersPart2)
    largo = numbersPart2.values.sum
    val formattedNumber = formatter.format(largo)
    println(s" ${formattedNumber} ${numbers.size} blink: ${i}")
  }
  println(s"Day 11 part 2: ${largo}") // 239321955280205


def processMap(list: Map[Long, Long]): List[(Long, Long)] = {
  list.toList // Convert the map to a list of tuples
    .sortBy { case (key, _) => key } // Sort by the first element of the tuple
}

def printMap(list: Map[Long, Long]): Unit =
  val processedList = processMap(list)
  println(s"blinked: ${processedList}\n")

def blink(prevMap: Map[Long, Long]): Map[Long, Long] = {
  val newMap = mutable.Map[Long, Long]()
  prevMap.foreach { case (value, rep) =>
    val strsome = toStringMap.get(value)
    var length = -1
    var str ="-"
    strsome match {
      case Some(strsome) =>
        str = strsome
        length = str.length
      case None =>
        str = value.toString
        length = str.length
        toStringMap(value) = str
    }
    if (value == 0) {
      val newVal = 1
      cacheOrSet(1L, newMap, rep)
    } else if (length % 2 == 0) {
      val left = str.substring(0, length / 2).toLong
      cacheOrSet(left, newMap, rep)
      val right = str.substring(length / 2).toLong
      cacheOrSet(right, newMap, rep)
    } else {
      val nval = value * 2024
      cacheOrSet(nval, newMap, rep)
    }
    case(value, rep) => {
      println(s"fell off ${value} ${rep}")
    }
  }
  newMap.toMap
}


def cacheOrSet(value: Long, longToLong: mutable.Map[Long, Long], repetitions: Long): Unit =
  if (longToLong.contains(value)) {
    val oldRepetitions = longToLong(value)
    longToLong(value) = oldRepetitions + repetitions
  } else {
    longToLong(value) = repetitions
  }