package day18

import day17.Expected.{Exact, No, Substring}

import scala.math.{E, pow}
import java.text.NumberFormat
import java.util.Locale
import scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.{Random, boundary}
import scala.util.boundary.break

var debug = true
def printDebug(str: String): Unit =
  if (debug) {
    println(str)
  }

var max = 12
var side = 6
var recursiveCalls = 0


def pathUpTo(ini: (Int, Int), fileContents: Array[String], upTo: Int): mutable.Map[(Int, Int), Int] = {
  val bytes = readBytesMap(fileContents, upTo)
  flood(bytes, side, ini, 0)
  return bytes
}
@main def main(): Unit =
  val startTime = System.nanoTime()
  val fileName = "inputs/day18/input.txt"
  if (fileName.contains("input.txt") || fileName.contains("challenge")) {
    debug = false
    side = 70
    max = 1024
  }
  val ini = (0,0)
  val fin = (side, side)

  val fileContents = Source.fromFile(fileName).getLines().toArray
  println(s"lines: ${fileContents.length} max: ${max} from ${ini} to ${fin}")
  println(s"last byte: ${fileContents(max-1)}")
  val bytes = pathUpTo(ini, fileContents, max)
  val endTime = System.nanoTime()
  val duration = (endTime - startTime) / 1e6
  println(s"recursive calls: ${recursiveCalls}")
  println(f"Execution time: $duration%.3f ms")
  println(s"Day 18 part 1 cost: ${bytes(fin)}") //370

  //Part 2. Let's brute force backwards, since djikstra is _faster_ with more obstacles
  val startTime2 = System.nanoTime()
  var last_one = ""
  boundary {
    for (count <- fileContents.length - 1 until 1024 by -1) {
      last_one = fileContents(count)
      val map = pathUpTo(ini, fileContents, count)
      if (map.contains(fin)) {
        break()
      }
    }
  }
  println(s"Day 18 part 2: last one: ${last_one}") // 65,6
  val endTime2 = System.nanoTime()
  val duration2 = (endTime2 - startTime2) / 1e6
  println(f"Execution time: $duration2%.3f ms")


val dirs = List((0,-1), (1,0), (0,1), (-1,0))

def flood(map: mutable.Map[(Int, Int), Int], side: Int, ini: (Int, Int), cost: Int): Unit =
  recursiveCalls += 1
  printDebug(s"map: ${map.size} @ ${ini} cost: ${cost}")
  if (!map.contains(ini) || (map(ini) >= 0 && map(ini) > cost)) {
    map(ini) = cost
  }
  val nextDirs = dirs.map{ case(dx, dy) => (ini._1 + dx, ini._2 + dy) }
  nextDirs.foreach{ case next =>
    if (next._1 >= 0 && next._2 >= 0 && next._1 <= side && next._2 <= side) {
      if (!map.contains(next) || map(next) > cost+1) {
        flood(map, side, next, cost + 1)
      }
    }
  }

def readBytesMap(strings: Array[String], upTo: Int): mutable.Map[(Int, Int), Int] =
  val result = mutable.Map[(Int, Int), Int]()
  var count = 0
  boundary {
    strings.foreach { line =>
      if (count >= upTo) {
        break()
      }
      val parts = line.split(",")
      result((parts(0).toInt, parts(1).toInt)) = -1
      count += 1
    }
  }
  result