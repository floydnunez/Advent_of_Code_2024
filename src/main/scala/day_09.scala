package day09

import day07.Oper.{add, mul}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.util.boundary
import scala.util.boundary.break
import scala.util.matching.Regex

@main def main(): Unit =
  val fileName = "inputs/day09/input.txt"
  val fileContents = Source.fromFile(fileName).getLines().toArray

  var line = fileContents(0)
  println((line, ";"))

  val sum = line.map(_.asDigit).sum
  println(s"sum: ${sum}")

  var filesystem: Array[Int] = Array.fill(sum)(-1)
  var filesystem_2: Array[Int] = Array.fill(sum)(-1)

  printArray(filesystem)
  fillArray(filesystem, line)
  fillArray(filesystem_2, line)
  printArray(filesystem)
//  println(filesystem.toList)
  //  the slow slow process of defragging
  defrag(filesystem)
  printArray(filesystem)
  val checksum = calcChecksum(filesystem)
  println(s"day 9 part 1 checksum: ${checksum}") // 6398608069280
  //part 2
  var (files, spaces) = calcFilePosAndSizes(filesystem_2)
//  printSubArr(files)
//  printSubArr(spaces)

  var moved = Set[Int]()
  for (i <- (files.length - 1) to 0 by -1) {
//    printSubArr(files)
//    printSubArr(spaces)
    var file = files(i)
//    println(s"checking on: ${file.toList}")
    var actuallyMoved = false
    boundary {
      spaces.foreach { space =>
        if (!(moved.contains(file(0))) && file(2) <= space(1) && file(1) > space(0)) { //fits and is a move to the left
          moved += file(0)
          actuallyMoved = true
//          println(s"moving ${file.toList} to ${space(0)}")
          for (jj <- 0 until file(2)) {
            filesystem_2(jj + space(0)) = file(0)
            filesystem_2(jj + file(1)) = -1
          }
          file(1) = space(0)
          break()
        }
      }
    }
    if (actuallyMoved) {
      var (_, newSpaces) = calcFilePosAndSizes(filesystem_2)
      spaces = newSpaces
    }
  }
//  printArray(filesystem)
  val checksum2 = calcChecksum(filesystem_2)
  println(s"day 9 part 2 checksum: ${checksum2}") // 6427437134372


def printSubArr(array: Array[Array[Int]]): Unit =
  println(array.map(_.mkString("(", ", ", ")")).mkString("(", ", ", ")"))

def calcFilePosAndSizes(filesystem: Array[Int]): (( Array[Array[Int]], Array[Array[Int]] )) =
  var resultFiles = ArrayBuffer[Array[Int]]()
  var current = Array(-1, -1, -1)
  var resultSpaces = ArrayBuffer[Array[Int]]()
  var currSpace = Array(-1,-1)
  var fillingSpace = false
  filesystem.zipWithIndex.foreach { case(value, index) =>
    fillingSpace = value < 0
    if (current(0) == -1 && value >= 0) {
      current(0) = value
      current(1) = index
    }
    if (current(0) != value) {
      current(2) = index - current(1)
      resultFiles += current
      current = Array(-1, -1, -1)
      if (value != -1) {
        current(0) = value
        current(1) = index
      }
    }
    if (currSpace(0) == -1 && fillingSpace) {
      currSpace(0) = index
    }
    if (fillingSpace) {
      currSpace(1) = index - currSpace(0) + 1
    }
    if (!fillingSpace && currSpace(0) >= 0) {
      resultSpaces += currSpace
      currSpace = Array(-1,-1)
    }
  }
  current(2) = filesystem.length - current(1)
  if (current(0) >= 0) {
    resultFiles += current
  }
//  currSpace(1) = filesystem.length - currSpace(1)
  if (currSpace(0) >= 0) {
    resultSpaces += currSpace
  }

  return ((resultFiles.toArray, resultSpaces.toArray))

def defrag(filesystem: Array[Int]): Unit = {
  while (hasADotInBetween(filesystem)) {
    defragOne(filesystem)
  }
}

def calcChecksum(ints: Array[Int]): Long =
  var total = 0L
  ints.zipWithIndex.foreach{ case(file, index) =>
    if (file >= 0) {
      total += (file * index).toLong
    }
  }
  return total

def defragOne(ints: Array[Int]): Unit =
  var file = -1
  boundary {
    for (i <- (ints.length - 1) to 0 by -1) {
      val thisFile = ints(i)
      if (thisFile >= 0) {
        ints.zipWithIndex.foreach { case (value, index) =>
          if (value == -1) {
            ints(index) = thisFile
            ints(i) = -1
            break()
          }
        }
      }
    }
  }

def hasADotInBetween(ints: Array[Int]): Boolean =
  var hasDot = false
  ints.foreach { file =>
    if (file == -1) {
      hasDot = true
    } else if (hasDot && file >= 0) {
      return true
    }
  }
  return false

def fillArray(ints: Array[Int], str: String): Unit =
  var intIndex = 0
  str.zipWithIndex.foreach{ case(value, index) =>
    val numValue = value.asDigit
    var tofill = -1
    if (index % 2 == 0) { //even
      tofill = index / 2
    }
    for(i <- 0 until numValue) {
//      println(s"filling at ${intIndex + i} with ${tofill}")
      ints(intIndex + i) = tofill
    }
    intIndex += numValue
  }
  return ints

def printArray(ints: Array[Int]): Unit =
  var str = ""
  ints.foreach{ elem =>
    if (elem < 0) {
      str += "."
    } else {
      str += elem
    }
  }
  println(str)