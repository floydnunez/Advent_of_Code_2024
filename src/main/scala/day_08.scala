package day08

import day07.Oper.{add, mul}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.util.boundary
import scala.util.boundary.break
import scala.util.matching.Regex

enum Oper {
  case add, mul
}

enum Oper2 {
  case add, mul, cat
}

@main def main(): Unit =
  val fileName = "inputs/day08/input.txt"
  val fileContents = Source.fromFile(fileName).getLines().toArray

  val height = fileContents.length
  val width = fileContents(0).length

  var data = ListBuffer[List[Long]]()

  var nodes = mutable.Map[String, List[List[Int]]]()
  fileContents.zipWithIndex.foreach { case(line, y) =>
    line.zipWithIndex.foreach{ case(char, x) =>
      val str = char.toString
      if (str != ".") {
        println(s"str: ${x}, ${y} = ${str}")
        nodes(str) = nodes.getOrElse(str, List()) :+ List(x, y)
      }
    }
  }

  var antinodes = mutable.Set[List[Int]]()
  var antinodes2 = mutable.Set[List[Int]]()
  nodes.foreach { case(key, list) =>
    println(s"${key}: ${list}")
    val pairs = list.combinations(2) //I assume this removes single antennas?
    pairs.foreach { case List(list1, list2) =>
      val antinode1 = generateAntinode(list1, list2)
      val antinode2 = generateAntinode(list2, list1)
      if (validAntinode(antinode1, width, height)) {
        antinodes += antinode1
      }
      if (validAntinode(antinode2, width, height)) {
        antinodes += antinode2
      }
      antinodes2 ++= generateAntinodes(list1, list2, width, height)
      antinodes2 ++= generateAntinodes(list2, list1, width, height)
      //the antinodes can naturally appear on the original antennas position
      //let's not calculate that, just add them
      antinodes2 += list1
      antinodes2 += list2
    }
  }
  println(antinodes)
  println(s"day 08, part 1: ${antinodes.size}") // 367
  println(antinodes2)
  println(s"day 08, part 2: ${antinodes2.size}") // 1285

def generateAntinodes(list1: List[Int], list2: List[Int], width: Int, height: Int): List[List[Int]] =
  var result = ListBuffer[List[Int]]()
  boundary {
    var prev = list1
    var curr = list2
    while(true) {
      val ints = generateAntinode(prev, curr)
      if (validAntinode(ints, width, height)) {
        result += ints
        prev = curr
        curr = ints
      } else {
        break()
      }
    }
  }
  return result.toList

def validAntinode(ints: List[Int], width: Int, height: Int): Boolean =
  if (ints.length != 2) {
    return false
  }
  if (ints(0) < 0 || ints(1) < 0 || ints(0) >= width || ints(1) >= height) {
    return false
  }
  return true

def generateAntinode(a: List[Int], b: List[Int]): List[Int] =
//  println(s"generating for ${a} and ${b}")
  val diffx = a(0) - b(0)
  val diffy = a(1) - b(1)
  var result = ListBuffer[Int]()
  result += b(0) - diffx
  result += b(1) - diffy
//  println(s" = ${result}")
  return result.toList