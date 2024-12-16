package day16

import java.text.NumberFormat
import java.util.Locale
import scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.boundary
import scala.util.boundary.break

case class Pos(var x: Int, var y: Int)

case class Node(x: Int, y: Int, costs: mutable.Map[Char, Int], var char: Char)

val dirs = Map('^' -> (0,-1), '>' -> (1,0), 'v' -> (0,1), '<' -> (-1,0))
val cardinal = List('^', '>', 'v', '<')

var height = 0
var width = 0
var debug = true

def initCostMap(): mutable.Map[Char, Int] =
  val result = mutable.Map('^' -> Int.MaxValue, '>' -> Int.MaxValue, 'v' -> Int.MaxValue, '<' -> Int.MaxValue)
  result

@main def main(): Unit =
  val fileName = "inputs/day16/input.txt"
  if (fileName == "inputs/day16/input.txt") {
    debug = false
  }
  val fileContents = Source.fromFile(fileName).getLines().toArray

  width = fileContents(0).length
  height = fileContents.length
  val map = mutable.Map[(Int, Int), Node]()
  val ini = Pos(0,0)
  val fin = Pos(0,0)
  fileContents.zipWithIndex.foreach{ case(line, yindex) =>
    line.zipWithIndex.foreach{ case(char, xindex) =>
      map((xindex, yindex)) = Node(x=xindex, y=yindex, costs = initCostMap(), char = char)
      if (char == 'S') {
        ini.x = xindex
        ini.y = yindex
        map((xindex, yindex)).char = '.' //easier later
      } else if (char == 'E') {
        fin.x = xindex
        fin.y = yindex
      }
    }
  }
  printMap(map)
  println(s"${width} x ${height}. From ${ini} to ${fin}")
  flood(map, '>', (ini.x, ini.y), 0)
  println(map((fin.x, fin.y)))
  println(s"\nDay 16 Part 1: ${map((fin.x, fin.y)).costs.values.min}") //88416

def printDebug(str: String): Unit =
  if (debug) {
    println(str)
  }

def flood(map: mutable.Map[(Int, Int), Node], dir: Char, pos: (Int, Int), cost: Int): Unit =
  val currNode = map(pos)
  calcCosts(currNode, dir, cost)
  println(s"costs calculated! ${currNode}")
  val nextPos = dirs.view.mapValues { case (dx, dy) => (pos._1 + dx, pos._2 + dy) }.toMap
  nextPos.foreachEntry{ case(nextDir, npos) =>
//    printDebug(s"? ${nextDir} => ${npos}")
    val nextNode = map(npos)
    if (nextNode.char == '.' || nextNode.char == 'E') {
      printDebug(s"nn costs dir: ${nextNode.costs(nextDir)} > curr node costs? ${currNode.costs(nextDir) + 1}")
      if (nextNode.costs(nextDir) > currNode.costs(nextDir) + 1) {
        flood(map, nextDir, npos, currNode.costs(nextDir) + 1)
      }
    }
  }

def calcCosts(node: Node, dir: Char, cost: Int):Unit =
  cardinal.foreach{ case cardinalDir =>
    if (cardinalDir == dir) {
      if (node.costs(dir) > cost) {
        printDebug(s"same direction! ${dir}")
        node.costs(cardinalDir) = cost
      }
    } else {
      val currDirIndex = cardinal.indexOf(dir)
      val cardinalDirIndex = cardinal.indexOf(cardinalDir)
      printDebug(s" is this a 90 degrees turn? ${dir} to ${cardinalDir} ${currDirIndex} to ${cardinalDirIndex} abs diff ${Math.abs(currDirIndex - cardinalDirIndex)}")
      printDebug(s"condition if: ${Math.abs(currDirIndex - cardinalDirIndex) == 1 || (currDirIndex == 0 && cardinalDirIndex == 3)|| (currDirIndex == 3 && cardinalDirIndex == 0) }")
      if (Math.abs(currDirIndex - cardinalDirIndex) == 1
        || (currDirIndex == 0 && cardinalDirIndex == 3)
        || (currDirIndex == 3 && cardinalDirIndex == 0) ) {
        if (node.costs(cardinalDir) > cost) {
          printDebug(s"90 degrees! ${dir} to ${cardinalDir}")
          node.costs(cardinalDir) = cost + 1000
        }
      } else {
        if (node.costs(cardinalDir) > cost) {
          printDebug(s"180 degrees! ${dir} to ${cardinalDir}")
          node.costs(cardinalDir) = cost + 2000
        }
      }
    }
  }

def printMap(map: mutable.Map[(Int, Int), Node]): Unit =
  var str = ""
  for( y <- 0 until height) {
    for( x <- 0 until width) {
      str += map((x,y)).char
    }
    str += "\n"
  }
  println(str)