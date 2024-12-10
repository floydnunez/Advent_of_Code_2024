package day07

import day07.Oper.{add, mul}

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
  val fileName = "inputs/day07/input.txt"
  val fileContents = Source.fromFile(fileName).getLines().toArray

  val data = ListBuffer[List[Long]]()

  fileContents.foreach { line =>
    val colon = line.split(":")
    val nums = colon(1).split(" ")
    val listBuffer = ListBuffer[Long]()
    listBuffer += colon(0).toLong
    println(nums.tail.toList)
    listBuffer ++= nums.tail.map(_.toLong)
    data += listBuffer.toList
  }
  println(data)
  var total = 0.toLong
  var total_2 = 0.toLong
  data.foreach { line =>
    val expectedResult = line(0)
    val perms = generatePermutations(line.length - 2)
    val perms2 = generatePermutations2(line.length - 2)
    boundary {
      perms.foreach { perm =>
        val result = evaluate(line.tail, perm)
        if (result == expectedResult) {
          total += expectedResult
          break()
        }
      }
    }
    boundary {
      perms2.foreach { perm =>
        val result = evaluate2(line.tail, perm)
        if (result == expectedResult) {
          total_2 += expectedResult
          break()
        }
      }
    }
  }
  println(s"Day 06 Part 1: ${total}") // 303766880536
  println(s"Day 06 Part 2: ${total_2}") // 303766880536
  // part 2

def evaluate(ints: List[Long], list: List[Oper]): Long =
  var operIndex = 0
  var prev = ints(0)
  ints.tail.foreach{ num =>
    val oper = list(operIndex)
    if (oper == Oper.add) {
      prev = prev + num
    } else if (oper == Oper.mul) {
      prev = prev * num
    }
    operIndex += 1
  }
  return prev

def evaluate2(ints: List[Long], list: List[Oper2]): Long =
  var operIndex = 0
  var prev = ints(0)
  ints.tail.foreach { num =>
    val oper = list(operIndex)
    if (oper == Oper2.add) {
      prev = prev + num
    } else if (oper == Oper2.mul) {
      prev = prev * num
    } else if (oper == Oper2.cat) {
      prev = (prev.toString + num.toString).toLong
    }
    operIndex += 1
  }
  return prev

def generatePermutations(x: Int): List[List[Oper]] = {
  val operations = List(Oper.add, Oper.mul)
  List.fill(x)(operations).flatten.combinations(x).flatMap(_.permutations).toList
}
def generatePermutations2(x: Int): List[List[Oper2]] = {
  val operations = List(Oper2.add, Oper2.mul, Oper2.cat)
  List.fill(x)(operations).flatten.combinations(x).flatMap(_.permutations).toList
}