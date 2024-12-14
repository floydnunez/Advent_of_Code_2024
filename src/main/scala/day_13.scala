package day13

import day07.Oper.{add, mul}
import org.apache.commons.math3.linear.ArrayRealVector

import java.text.NumberFormat
import java.util.Locale
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.util.boundary
import scala.util.boundary.break
import scala.util.matching.Regex
import org.apache.commons.math3.optim.linear.*
import org.apache.commons.math3.optim.nonlinear.scalar.GoalType
import org.apache.commons.math3.optim.OptimizationData
import org.apache.commons.math3.optim.PointValuePair

import scala.collection.JavaConverters.seqAsJavaListConverter
import scala.collection.JavaConverters.asJavaIterableConverter

case class Button(name: String, x: Long, y: Long)
case class Prize(var x: Long, var y: Long)

case class Problem(a: Button, b: Button, prize: Prize)
case class Solution(aPresses: Long, bPresses: Long, cost: Long, problem: Problem)

@main def main(): Unit =
  val fileName = "inputs/day13/input.txt"
  val fileContents = Source.fromFile(fileName).getLines().toArray

  println(fileContents)

  val listProblemsMut = ListBuffer[Problem]()

  //do note that sliding requires a couple of extra newlines at the end of the file
  fileContents.sliding(4,4).foreach { case Array(a, b, prize, newline) =>
    val buttonA = parseButton("A", a)
    val buttonB = parseButton("B", b)
    val prizeP = parsePrize(prize)
    listProblemsMut += Problem(a=buttonA, b=buttonB, prize = prizeP)
  }
  val listProblems = listProblemsMut.toList
  println(listProblems.length)

  val listAllSolutions=ListBuffer[Solution]()
  val listAlgebraSolutions = ListBuffer[Solution]()

  val objectiveCoefficients = Array(3.0, 1.0) // Cost for Button A and Button B

  var count = 0
  listProblems.foreach { case p =>
    val solutions = ListBuffer[Solution]()
    count += 1
    for (ii <- 0 until 100) {
      for (jj <- 0 until 100) {
        val xx = p.a.x * ii + p.b.x * jj
        val yy = p.a.y * ii + p.b.y * jj
        if (xx == p.prize.x && yy == p.prize.y) {
          solutions += Solution(aPresses = ii, bPresses = jj, cost = 3 * ii + jj, p)
          if (count ==321) {
            println(s"last problem: ${ii} ${jj}")
          }
        } else {
          if (count == 321) {
            println(s"problem-: ${p}")
          }
        }
      }
    }
    var lowestCostSolution: Option[Solution] = None
    if (!solutions.isEmpty) {
      val sol = solutions.minBy(_.cost)
      lowestCostSolution = Some(sol)
      listAllSolutions += sol
    } else {
      lowestCostSolution = None
    }
  }

  val total_1 = listAllSolutions.map(_.cost).sum

  println(s"Day 13 part 1: ${total_1}") // 29201
  println(s"cuantas soluciones normales? ${listAllSolutions.length}")

  var total_2 = 0L
  var algebraicSolutions = 0

  listProblems.foreach { prob =>
    prob.prize.x += 10000000000000L
    prob.prize.y += 10000000000000L

    val Ax = prob.a.x.toDouble
    val Ay = prob.a.y.toDouble
    val Bx = prob.b.x.toDouble
    val By = prob.b.y.toDouble
    val Px = prob.prize.x.toDouble
    val Py = prob.prize.y.toDouble

    try {
      // had to solve this. Thanks to my teachers in high school
      val B = (Py - (Ay * Px / Ax)) / (By - (Ay * Bx / Ax))
      val A = Px / Ax - Bx * B / Ax

      if ( isInteger(A) && isInteger(B) ) {
        val lA = Math.round(A)
        val lB = Math.round(B)
        val cost:Long = 3 * lA + lB
        total_2 += cost
        algebraicSolutions += 1
        println(s"Solucion entera!: ${lA} ${lB} = ${cost}")
      } else {
        if (almostInteger(A) || almostInteger(B)) {
          println("close solution!")
        }
        val cost = 3*A+B
        println(s"Solucion no entera!: ${A} ${B} = ${cost}")
      }
    }catch
      case ex: Exception => println(s"\n\nsolucion algebraica invalida? ${ex} \n\n")
  }
  println(s"Day 13 part 2: ${total_2}") // 104140871044942
  println(s"total soluciones: ${algebraicSolutions} of ${listProblems.length}")

def isInteger(d: Double): Boolean =
  val round = Math.round(d)
  Math.abs(round - d) < 0.05 // turns out this is a good enough threshold. Thanks Santa

def almostInteger(d: Double): Boolean =
  val round = Math.round(d)
  Math.abs(round - d) < 0.1 // turns out this is a good enough threshold. Thanks Santa

def parsePrize(str: String): Prize =
  val parts = str.split(" ")
  val x = parts(1).split("=")(1).split(",")(0)
  val y = parts(2).split("=")(1)
  val prize = Prize(x=x.toInt, y=y.toInt)
  println(prize)
  return prize

def parseButton(name: String, str: String): Button =
  val parts = str.split(" ")
  val xplus = parts(2)
  val xxx = xplus.split("\\+")
  val x = xxx(1).split(",")(0)
  val yplus = parts(3)
  val y = yplus.split("\\+")(1)
  val bname = parts(1).split(":")(0)
  println(s"${name} ${x} ${y}")
  if (name!=bname) {
    println(s"name: ${name} bname: ${bname}")
    throw new Exception("Names don't match")
  }
  val button = Button(name=name, x=x.toInt, y=y.toInt)
  println(button)
  return button