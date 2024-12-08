package day06

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.util.boundary
import scala.util.boundary.break
import scala.util.matching.Regex

enum Dirs {
  case up, right, down, left
}

val diffs = ArrayBuffer[List[Int]]()
@main def main(): Unit =
  diffs += List(0, -1)
  diffs += List(1, 0)
  diffs += List(0, 1)
  diffs += List(-1, 0)

  val fileName = "inputs/day06/input.txt"
  val fileContents = Source.fromFile(fileName).getLines().toArray

  var x = 0
  var widthSet = 0
  var y = 0
  val height = fileContents.length
  fileContents.zipWithIndex.foreach{ case(line, index) =>
    widthSet = line.length
    println(line)
    val xindex = line.indexOf("^")
    if (xindex >= 0) {
      x = xindex
      y = index
    }
  }
  val originX = x
  val originY = y
  val width = widthSet
  println(s"${x} ${y}")
  import Dirs._

  println(s"${diffs(up.ordinal)}")
  println(s"${diffs(right.ordinal)}")
  println(s"${diffs(down.ordinal)}")
  println(s"${diffs(left.ordinal)}")

  val map = ArrayBuffer.from(fileContents)

  val (looped, cmap) = checkMap(map, x, y, width, height)
  println(looped)
  val countX = cmap.map(_.count(_ == 'X')).sum
  println(s"part 1 ${countX}") // 5177
  // part 2
  var total_2 = 0
  var indexY = 0
  while(indexY < height) {
    var indexX = 0
    while(indexX < width) {
      val mapReplaced = ArrayBuffer.from(fileContents)
      val same = replaceWith(mapReplaced, indexX, indexY, "#")
//      println(s"same? ${same} ${indexX} ${indexY}")
      if (!same) {
        val (looped, cmap) = checkMap(mapReplaced, x, y, width, height)
//        println(s"loop? ${looped} ${indexX} ${indexY}")
        if (looped) {
          total_2 += 1
        }
      }
      indexX += 1
    }
    indexY += 1
    println(s"until height ${indexY} of ${height} partial total: ${total_2}")
  }
  println(s"part 2: ${total_2}")

def checkMap(string: ArrayBuffer[String], xx: Int, yy: Int, width: Int, height: Int):
  (Boolean, ArrayBuffer[String]) =
  import Dirs._
  var story = ArrayBuffer[(Dirs, Int, Int)]()
  story += ((up, xx, yy))
  val map = ArrayBuffer.from(string)
//  map.foreach(line=>println(line))
  var currDir = up
  var x = xx
  var y = yy
  var loops = false
  boundary {
    while (x >= 0 && y >= 0 && x < width && y < height) {
      val changedP = replaceWith(map, x, y, "X")
      val step = diffs(currDir.ordinal)
      x += step(0)
      y += step(1)
      val square = getSquareAt(map, x, y, width, height)
      if (square == "#") {
        //undo step
        x -= step(0)
        y -= step(1)
        if (currDir == up) {
          currDir = right
        } else if (currDir == right) {
          currDir = down
        } else if (currDir == down) {
          currDir = left
        } else if (currDir == left) {
          currDir = up
        }
      }
      val currStep = ((currDir, x, y))
      if (story.contains(currStep)) {
//        println("LOOPSloops loops")
        loops = true
        break()
      }
      story += currStep
    }
  }
//  println(s"story: ${story.length}")
  return (loops, map)

def getSquareAt(strings: ArrayBuffer[String], x: Int, y: Int, width: Int, height: Int): String =
  if (x < 0 || y < 0 || x >= width || y >= height) {
    return "N"
  }
  return strings(y).substring(x,x+1)

def replaceWith(strings: ArrayBuffer[String], x: Int, y: Int, replacement: String): Boolean =
  val original = strings(y).substring(x, x+1)
  if (original == replacement) {//no change
    return true
  }
  strings(y) = strings(y).substring(0,x) + replacement + strings(y).substring(x+1)
  return false