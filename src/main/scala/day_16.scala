package day16

import java.text.NumberFormat
import java.util.Locale
import scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.boundary
import scala.util.boundary.break

case class Pos(var x: Int, var y: Int)

case class Node(x: Int, y: Int, costs: mutable.Map[Char, Int], var char: Char, var isBest: Boolean)
var recursiveCalls = 0
val dirs = Map('^' -> (0,-1), '>' -> (1,0), 'v' -> (0,1), '<' -> (-1,0))
val opposite = Map('^' -> 'v', '>' -> '<', 'v' -> '^', '<' -> '>')
val cardinal = List('^', '>', 'v', '<')

var height = 0
var width = 0
var debug = true

def initCostMap(): mutable.Map[Char, Int] =
  val result = mutable.Map('^' -> Int.MaxValue, '>' -> Int.MaxValue, 'v' -> Int.MaxValue, '<' -> Int.MaxValue)
  result

@main def main(): Unit =
  val fileName = "inputs/day16/input.txt"
  if (fileName == "inputs/day16/input.txt" || fileName.contains("challenge")) {
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
      map((xindex, yindex)) = Node(x=xindex, y=yindex, costs = initCostMap(), char = char, isBest = false)
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
  val endNode = map((fin.x, fin.y))
//  println(endNode)
  val minCost = endNode.costs.values.min
  println(s"\n\nDay 16 Part 1: ${minCost}") //88416
  println(s"recursive calls: ${recursiveCalls}")
  //Part 2. Let's backtrack
  println("\n\n\n\n\n\n\n\n------------------------------------------------\n\n\n\n\n\n\n\n")
//  debug = true
  backtrackFrom(map, (endNode.x, endNode.y), Int.MaxValue)
  printMapPath(map, minCost)
  val bestNodesCount = map.values.count(_.isBest)
  println(s"\nDay 16 Part 1: ${minCost}") //88416
  println(s"\nDay 16 Part 2: ${bestNodesCount}") //442

def backtrackFrom(map: mutable.Map[(Int, Int), Node], pos: (Int, Int), prevCost: Int): Unit = {
  if (!map.contains(pos)) {
    return
  }
  val currNode = map(pos)
  if (currNode.char == '#') {
    return
  }
  printDebug(s"Backtracking on ${currNode} prevCost: ${prevCost} on ${currNode.costs}")
  currNode.isBest = true
  val minCost = currNode.costs.values.min
  if (minCost == 0) { //the end! ...or start
    return
  }
  var count = 0
  currNode.costs.foreachEntry { case (dir, cost) => {
    printDebug(s"-for each entry of node ${pos}: ${dir} ${cost}")
      if (cost == minCost) {
        count += 1
        val nextDir = dirs(opposite(dir))
        val nextPos = (pos._1 + nextDir._1, pos._2 + nextDir._2)
        printDebug(s"   min dir: ${dir}, from: ${nextPos} to ${pos} ")
        backtrackFrom(map, nextPos, minCost)
      } else if (cost != Int.MaxValue) {
        val nextDir = dirs(opposite(dir))
        val nextPos = (pos._1 + nextDir._1, pos._2 + nextDir._2)
        val nextNode = map(nextPos)
        printDebug(s"   not min: ${dir} ${nextPos} ${nextNode.char} ${nextNode.costs}")
        if (nextNode.char != '#') { //not a wall
          val minNextCost = nextNode.costs.values.min
          if (minNextCost + 2 == prevCost && !nextNode.isBest) {
            //hack? just jump over if prev cost is 2 more than min next cost, ignoring the discontinuity.
            //It happens on node (5,7) on example_1
            printDebug("trust the bifurcation")
            backtrackFrom(map, nextPos, minCost)
          }
        }
      }
      if (count > 1) {
        printDebug(s"bifurcation at ${pos}")
      }
    }
  }
}

def printDebug(str: String): Unit =
  if (debug)
    println(str)

def floodBreadth(map: mutable.Map[(Int, Int), Node], startDir: Char, startPos: (Int, Int), startCost: Int): Unit = {
  val queue = mutable.Queue[(Char, (Int, Int), Int)]() // (Direction, Position, Cost)
  queue.enqueue((startDir, startPos, startCost)) // Start with the initial position

  while (queue.nonEmpty) {
    val (dir, pos, cost) = queue.dequeue() // Get the current node from the queue
    val currNode = map(pos)
    recursiveCalls += 1

    calcCosts(currNode, dir, cost) // Update costs
    printDebug(s"costs calculated! ${currNode}")

    // Calculate neighbors
    val nextPos = dirs.view.mapValues { case (dx, dy) => (pos._1 + dx, pos._2 + dy) }.toMap
    nextPos.foreachEntry { case (nextDir, npos) =>
      printDebug(s"  trying ${nextDir} => ${npos}")
      if (map.contains(npos)) {
        val nextNode = map(npos)
        if (nextNode.char == '.' || nextNode.char == 'E') {
          printDebug(s"   nn costs dir: ${nextNode.costs(nextDir)} > curr node costs? ${currNode.costs(nextDir) + 1}")
          if (nextNode.costs(nextDir) > currNode.costs(nextDir) + 1) {
            // Update the cost and enqueue the neighbor for further processing
            nextNode.costs(nextDir) = currNode.costs(nextDir) + 1
            queue.enqueue((nextDir, npos, currNode.costs(nextDir) + 1))
          }
        }
      }
    }
  }
}

def floodDepth(map: mutable.Map[(Int, Int), Node], startDir: Char, startPos: (Int, Int), startCost: Int): Unit = {
  val stack = mutable.Stack[(Char, (Int, Int), Int)]() // (Direction, Position, Cost)
  stack.push((startDir, startPos, startCost)) // Start with the initial position

  while (stack.nonEmpty) {
    val (dir, pos, cost) = stack.pop() // Get the current node from the stack
    val currNode = map(pos)
    recursiveCalls += 1

    calcCosts(currNode, dir, cost) // Update costs
    printDebug(s"costs calculated! ${currNode}")

    // Calculate neighbors
    val nextPos = dirs.view.mapValues { case (dx, dy) => (pos._1 + dx, pos._2 + dy) }.toMap
    nextPos.foreachEntry { case (nextDir, npos) =>
      printDebug(s"  trying ${nextDir} => ${npos}")
      if (map.contains(npos)) {
        val nextNode = map(npos)
        if (nextNode.char == '.' || nextNode.char == 'E') {
          printDebug(s"   nn costs dir: ${nextNode.costs(nextDir)} > curr node costs? ${currNode.costs(nextDir) + 1}")
          if (nextNode.costs(nextDir) > currNode.costs(nextDir) + 1) {
            // Update the cost and push the neighbor onto the stack for further processing
            nextNode.costs(nextDir) = currNode.costs(nextDir) + 1
            stack.push((nextDir, npos, currNode.costs(nextDir) + 1))
          }
        }
      }
    }
  }
}


def flood(floodedmap: mutable.Map[(Int, Int), Node], dir: Char, pos: (Int, Int), cost: Int): Unit =
  recursiveCalls += 1
  val currNode = floodedmap(pos)
  calcCosts(currNode, dir, cost)
//  printDebug(s"costs calculated! ${currNode}")

  val nextPos = dirs.view.mapValues { case (dx, dy) => (pos._1 + dx, pos._2 + dy) }.toMap
  nextPos.foreachEntry { case (nextDir, npos) =>
    printDebug(s"  trying ${nextDir} => ${npos}")
    val nextNode = floodedmap(npos)
    if (nextNode.char == '.' || nextNode.char == 'E') {
      val newCost = currNode.costs(nextDir) + 1
      printDebug(s"   nn costs dir: ${nextNode.costs(nextDir)} > curr node costs? ${newCost}")
      if (nextNode.costs(nextDir) > newCost) {
          flood(floodedmap, nextDir, npos, newCost)
      }
    }
  }

def calcCosts(node: Node, dir: Char, cost: Int):Unit =
//  printDebug(s"calc Costs: inicost: ${cost} ${dir} ${node}")
  cardinal.foreach{ case cardinalDir =>
//    printDebug(s" trying ${cardinalDir}")
    if (cardinalDir == dir) {
      if (node.costs(dir) > cost) {
//        printDebug(s"   same direction! ${dir}")
        node.costs(cardinalDir) = cost
      }
    } else {
      val currDirIndex = cardinal.indexOf(dir)
      val cardinalDirIndex = cardinal.indexOf(cardinalDir)
//      printDebug(s"   is this a 90 degrees turn? ${dir} to ${cardinalDir} ${currDirIndex} to ${cardinalDirIndex} abs diff ${Math.abs(currDirIndex - cardinalDirIndex)}")
      if (Math.abs(currDirIndex - cardinalDirIndex) == 1
        || (currDirIndex == 0 && cardinalDirIndex == 3)
        || (currDirIndex == 3 && cardinalDirIndex == 0) ) {
        if (node.costs(cardinalDir) > cost) {
//          printDebug(s"      90 degrees! ${dir} to ${cardinalDir}")
          node.costs(cardinalDir) = cost + 1000
        }
      } else {
        if (node.costs(cardinalDir) > cost) {
//          printDebug(s"      180 degrees! ${dir} to ${cardinalDir}")
          node.costs(cardinalDir) = cost + 2000
        }
      }
    }
  }

def printMap(map: mutable.Map[(Int, Int), Node]): Unit =
  var str = ""
  for (y <- 0 until height) {
    for (x <- 0 until width) {
      str += map((x, y)).char
    }
    str += "\n"
  }
  println(str)

def printMapPath(map: mutable.Map[(Int, Int), Node], minCost: Int): Unit =
  var minCostSize = minCost.toString.length
  var str = ""
  for( y <- 0 until height) {
    for( x <- 0 until width) {
      val node = map((x,y))
      val minCost = node.costs.values.min
      if (minCost == Int.MaxValue) {
        str += node.char.toString * (minCostSize + 1)
      } else {
        val formatString = s"%${minCostSize }d"
        val formattedNumber = String.format(formatString, minCost.asInstanceOf[Object])
        var isBest = " "
        if (node.isBest) {
          isBest = "_"
        }
        str += isBest
        str += s"${formattedNumber}"
      }
    }
    str += "\n"
  }
  println(str)