package day05

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.util.boundary
import scala.util.boundary.break
import scala.util.matching.Regex

@main def main(): Unit =
  val fileName = "inputs/day05/input.txt"
  val fileContents = Source.fromFile(fileName).getLines().toList
  println("File contents:")

  val pairs = ListBuffer[List[Int]]()
  val data = ListBuffer[String]()

  var isData = false
  fileContents.foreach( line => {
    if (line == "") {
      isData = true
    } else if (!isData) {
      val parts = line.split("\\|")
      val sub = ListBuffer[Int]()
      sub += parts(0).toInt
      sub += parts(1).toInt
      pairs += sub.toList
      println(s"sub to list: ${sub.toList}")
    } else {
      data += line
    }
  })
  val rules = pairs.toList
  val updates = data.toList
  println(rules)
  println(updates)
  //now lets generate a regex for each rule
  val regexes = rules.map(pair => s"${pair(1)},[\\d,]*${pair(0)}".r)
  println(regexes)
  var total = 0
  var total_2 = 0

  val wrongs = ListBuffer[List[Int]]()

  updates.foreach{ update =>
    var allRight = true
    regexes.foreach{ regex =>
      if (regex.findFirstIn(update).isDefined) {
        allRight = false
      }
    }
    val pages = update.split(",").map(_.toInt)
    if (allRight) {
      println(s"update ${update} is right")
      val mid = pages(pages.length/2)
      println(s"mid: ${update} => ${mid}")
      total += mid
    } else {
      wrongs += pages.toList
      println(s"update ${update} is wrong")
    }
  }

  println(s"total part 1:  ${total}") // 6951
  //part 2
  println(wrongs)
  val copyWrongs = ArrayBuffer.from(wrongs)
  println(copyWrongs)
  wrongs.zipWithIndex.foreach { case(wrong, index) =>
    var pairIndex = 0
    while(pairIndex < pairs.length){
      val pair = pairs(pairIndex)
      val regex = s"${pair(1)},[\\d,]*${pair(0)}".r
      val wrongStr = copyWrongs(index).mkString(",")
      if (regex.findFirstIn(wrongStr).isDefined) {
        println(s"for ${wrong} ${pair}")
        copyWrongs(index) = swapAround(copyWrongs(index), pair(0), pair(1))
        println(copyWrongs(index))
        pairIndex = 0 // try all over again. Cannot stop until all rules are right.
      }
      pairIndex += 1
    }
  }

  copyWrongs.zipWithIndex.foreach { case (wrong, index) =>
    pairs.foreach { pair =>
      val regex = s"${pair(1)},[\\d,]*${pair(0)}".r
      val wrongStr = copyWrongs(index).mkString(",")
      if (regex.findFirstIn(wrongStr).isDefined) {
        println(s"still ${wrong} ${pair}")
      }
    }
  }

  copyWrongs.foreach{ wrong =>
    val mid = wrong(wrong.length / 2)
    total_2 += mid
  }
  println(s"total part 2:  ${total_2}") // 4121

def swapAround(ints: List[Int], v1: Int, v2: Int): List[Int] =
  val i1 = ints.indexOf(v1)
  val i2 = ints.indexOf(v2)
  val result = ListBuffer[Int]()
  ints.zipWithIndex.foreach { case (value, index) =>
    if (index == i1) {
      result += v1
      result += v2
    } else if (index != i2){
      result += value
    }
  }
  return result.toList