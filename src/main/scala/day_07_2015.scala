package day07_2015

import day07.Oper.{add, mul}
import day07_2015.Oper.{AND, ASS, LSHIFT, OR}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.util.boundary
import scala.util.boundary.break
import scala.util.matching.Regex

enum Oper {
  case ASS, AND, OR, NOT, LSHIFT, RSHIFT
}

sealed trait Value
case class StringData(value: String) extends Value
case class IntData(value: Int) extends Value
case object NilData extends Value

@main def main(): Unit =
  val fileName = "inputs/day07_2015/input.txt"
  val fileContents = Source.fromFile(fileName).getLines().toArray

  var data = ListBuffer[List[Int]]()
  var nodes = mutable.Map[String, ((Oper, Value, Value))]()
  fileContents.foreach { line =>
    val arrowSplit = line.split(" -> ")
    val key = arrowSplit(1)
    var values = arrowSplit(0).split(" ")
    if (values.length == 1) { // easy case, just a value
      if (isInteger(values(0))){
        nodes(key) = ((Oper.ASS, IntData(values(0).toInt), NilData ))
      } else {
        nodes(key) = (Oper.ASS, StringData(values(0)), NilData )
      }
    }
    if (values.length == 2) { // unary op...only NOT so far
      println(s"value: ${values.toList};")
      if (isInteger(values(1))){
        nodes(key) = (Oper.NOT, IntData(values(1).toInt), NilData )
      } else {
        nodes(key) = (Oper.NOT, StringData(values(1)), NilData )
      }
      println(s"nodes: ${nodes};")
    }
    if (values.length == 3) {
        nodes(key) = (getOper(values(1)), getData(values(0)), getData(values(2)) )
    }
  }
//  println(("part 1: ", solveFor("a", nodes))) // 32767 too low
//  println(nodes)

//  println(("evaluation: ", solveFor("d", nodes)))
//  println(("evaluation: ", solveFor("e", nodes)))
//  println(("evaluation: ", solveFor("f", nodes)))
//  println(("evaluation: ", solveFor("g", nodes)))
//  println(("evaluation: ", solveFor("h", nodes)))
//  println(("evaluation: ", solveFor("i", nodes)))
  println(("evaluation: ", solveFor("a", nodes)))
//  println(("evaluation: ", solveFor("b", nodes)))
  println(nodes)

def solveFor(str: String, stringToTuple: mutable.Map[String, (Oper, Value, Value)]): Int =
  val formula = stringToTuple(str)

  def evaluate(value: Value): Int = value match {
    case StringData(innerStr) => solveFor(innerStr, stringToTuple) // Recursive call
    case IntData(num) => {
      stringToTuple(str) = ((ASS, IntData(unsign(num)), NilData))
      unsign(num)
    }// Return the Integer directly
    case NilData => 0
  }
  println(s"${str}: \t formula: ${formula}")
  formula match {
    case (Oper.AND, left, right) => unsign(evaluate(left)) & unsign(evaluate(right))
    case (Oper.OR, left, right) => unsign(evaluate(left)) | unsign(evaluate(right))
    case (Oper.NOT, left, NilData) => ~unsign(evaluate(left))
    case (Oper.ASS, left, NilData) => unsign(evaluate(left))
    case (Oper.RSHIFT, left, right) => unsign(unsign(evaluate(left)) >> unsign(evaluate(right)))
    case (Oper.LSHIFT, left, right) => unsign(unsign(evaluate(left)) << unsign(evaluate(right)))
  }

def unsign(i: Int): Int =
  if (i > 65535) {
    return 65535
  }
  if (i < 0) {
    return 65536 + i
  }
  return i

def getData(str: String): Value =
  if (isInteger(str)) {
    return IntData(str.toInt)
  }
  return StringData(str)

def getOper(string: String) : Oper =
  if (string == "AND") {
    return Oper.AND
  }
  if (string == "OR") {
    return Oper.OR
  }
  if (string == "LSHIFT") {
    return Oper.LSHIFT
  }
  if (string == "RSHIFT") {
    return Oper.RSHIFT
  }
  return Oper.ASS

def isInteger(s: String): Boolean = {
  s.matches("""\d+""")
}