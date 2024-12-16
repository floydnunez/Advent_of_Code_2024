package day15

import java.text.NumberFormat
import java.util.Locale
import scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.boundary
import scala.util.boundary.break

case class Robot(var x: Int, var y: Int)

val dirMap = Map( '^' -> (0, -1), '>' -> (1, 0), 'v' -> (0, 1), '<' -> (-1, 0))
var debug = true
var width = 0
var height = 0

@main def main(): Unit =
  val fileName = "inputs/day15/input.txt"
  if (fileName == "inputs/day15/input.txt") {
    debug = false
  }
  val fileContents = Source.fromFile(fileName).getLines().toArray

  width = fileContents(0).length

  val map = mutable.Map[(Int, Int), String]()
  var instructions = ""
  var isInstructions = false

  var y = 0
  val robot = Robot(x = -1, y = -1)
  fileContents.foreach{ line =>
    if (line == "") {
      isInstructions = true
    }
    if (isInstructions) {
      instructions += line
    } else {
      val parts = line.split("")
      parts.zipWithIndex.foreach{ case(letter, index) =>
        if (letter == "@") {
          robot.x = index
          robot.y = y
        }
        map((index, y)) = letter.toString
      }
      y += 1
    }
  }
  height = y
  printMap(map)
  println(s"instructions: ${instructions}")
  println(s"robot @ ${robot}")

  //part 1
  instructions.foreach{ dir =>
    move(map, robot, dir)
    if(debug) {
      printMap(map)
    }
  }
  println("\n\nfin\n\n")
  printMap(map)
  val total_1 = calcScore(map)
  println(s"Day 15 part 1: ${total_1}")

def calcScore(map: mutable.Map[(Int, Int), String]): Int =
  var score = 0
  map.foreach{ case(key, value) =>
    if (value == "O") {
      score += key._1 + key._2 * 100
    }
  }
  score

def move(map: mutable.Map[(Int, Int), String], robot: Robot, dir: Char): Unit =
  val exrobot = recursiveMove(map, (robot.x, robot.y), dir)
  if (exrobot != "@") {
    val dirdiff = dirMap(dir)
    robot.x += dirdiff._1
    robot.y += dirdiff._2
  }

def recursiveMove(map: mutable.Map[(Int, Int), String], from: (Int, Int), dir: Char): String =
  val dirdiff = dirMap(dir)
  val curr = map(from)
  if (debug) {
    println(s"moving ${curr} ${from} ${dir}")
  }
  val next = (from._1 + dirdiff._1, from._2 + dirdiff._2)
  if (!map.contains(next) || map(next) == "#") {
    if (debug) {
      println("fell outside or found wall. End")
    }
    return curr
  }
  if (map(next) == ".") {
    map(next) = curr
    map(from) = "."
    if (debug) {
      println("found space, switching around")
    }
    return "."
  }
  var recursiveNext = recursiveMove(map, next, dir)
  if (recursiveNext == ".") {
    map(next) = curr
    map(from) = "."
    if (debug) {
      println("found space, switching around")
    }
    return "."
  }
  return curr

def printMap(map: mutable.Map[(Int, Int), String]): Unit =
  var str = ""
  for( y <- 0 until height ) {
    for ( x <- 0 until width ) {
      str += map((x, y))
    }
    str += "\n"
  }
  println(str)
