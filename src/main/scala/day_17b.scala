package day17b

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
val formatter = NumberFormat.getInstance(Locale.GERMANY)

val regAInit: Long = 23_948_989L //226_304_000_061L

//PART 2 SOLUTION: 164541160582845

enum Expected {
  case No, Exact, Substring
}

case class Computer(var regA: Long, var regB: Long, var regC: Long, code: List[Int], var pointer: Int, output: ListBuffer[Int])

def printDebug(str: String): Unit =
  if (debug) {
    println(str)
  }

def compute(code: List[Int], regA: Long, regB: Long, regC: Long, expectedOutput: Option[List[Int]], maxSteps: Long): (List[Int], Expected, Long) = {
  val computer = Computer(regA = regA, regB = regB, regC = regC, code = code, pointer = 0, output = ListBuffer[Int]())
//  println(s"register A = ${regA} code: ${code} expecting to produce ${expectedOutput} maxsteps: ${maxSteps}" )
  var isWhatIExpected = Expected.No
  var steps = 0L
  val code_length = code.length
  while (computer.pointer < code_length && steps < maxSteps) {
    val instruction = code(computer.pointer)
    apply(instruction, computer)
    steps += 1
    if (expectedOutput.isDefined && instruction == 5 /* output */) {
      val expected = expectedOutput.get
      isWhatIExpected = compare(expected, computer.output)
      if (isWhatIExpected == Expected.No) {
        return (computer.output.toList, Expected.No, steps)
      }
    }
  }
  return (computer.output.toList, isWhatIExpected, steps)
}

def compare(expectedList: List[Int], actualListBuffer: ListBuffer[Int]): Expected =
  val actualList = actualListBuffer.toList
//  printDebug(s"comparing: ${expectedList} to ${actualList}")
  val ellength = expectedList.length
  val allength = actualList.length
  if (ellength == allength && expectedList == actualList) {
    return Expected.Exact
  } else if (ellength > allength && expectedList.startsWith(actualList)) {
    return Expected.Substring
  } else {
    return Expected.No
  }


@main def main(): Unit =
  val startTime = System.nanoTime()
  val fileName = "inputs/day17/input.txt"
  if (fileName.contains("input") || fileName.contains("challenge")) {
    debug = false
  }

  val fileContents = Source.fromFile(fileName).getLines().toArray

  val rA = fileContents(0).split(": ")(1).toInt
  val rB = fileContents(1).split(": ")(1).toInt
  val rC = fileContents(2).split(": ")(1).toInt

  val code = fileContents(4).split(": ")(1).split(",").toList.map(_.trim.toInt)
  val (outputList, _, p1steps) = compute(code, rA, rB, rC, None, 1000)
  println(s"steps ${p1steps}")
  println(s"result: ${outputList.toList}")
  println("")
  println(s"register A ${rA}")
  println(s"register B ${rB}")
  println(s"register C ${rC}")

  println(s"Day 17 Part 1: ${outputList.toList}") //List(6, 4, 6, 0, 4, 5, 7, 2, 7), registerA = 64196994, regB = 0, regC = 0

  //Part 2
  //example
  debug = false
  val (quine, isItTheSame, steps) = compute(code, rA, rB, rC, Some(List(6, 4, 6, 0, 4, 5, 7, 2, 7)), 1000)
  println(s"quine: ${quine} is it the expected val? ${isItTheSame} ran for ${steps} steps")

  var tryFor: Long = 1000
  var notQuineYet = true
  var replacedRegisterA: Long = regAInit
  while(notQuineYet) {
    replacedRegisterA += 268435456L // 10000000 in hex
    val (quine, isItTheSame, steps) = compute(code, replacedRegisterA, rB, rC, Some(code), tryFor)
    if (replacedRegisterA % (128 * 4_000_000) == 61) {
//      debug = false
      val formattedNumber = formatter.format(replacedRegisterA)
      println(s"register A: ${formattedNumber} ran for ${steps}")
    }
    isItTheSame match {
      case Expected.No => {
        printDebug(s"Failed with registerA ${replacedRegisterA} result ${quine} after running for ${steps} steps")
//        replacedRegisterA += 1
      }
      case Expected.Substring => {
        println(s"Got a substring (${quine}) with ${replacedRegisterA} after running for ${steps}")
//        replacedRegisterA += 1
      }
      case Expected.Exact => {
        println(s"   --->   Day 17 Part 2: ${replacedRegisterA} gives ${quine} from ${code} after running for ${steps} steps")
        notQuineYet = false //it is a quine!
      }
    }

    //      notQuineYet = false
  }

inline def apply(instruction: Int, computer: Computer): Unit =
  val operator = computer.code(computer.pointer+1)
  instruction match
  case 0 => adv(operator, computer)
  case 1 => bxl(operator, computer)
  case 2 => bst(operator, computer)
  case 3 => jnz(operator, computer)
  case 4 => bxc(operator, computer)
  case 5 => out(operator, computer)
  case 6 => bdv(operator, computer)
  case 7 => cdv(operator, computer)
  case _ => {
    println("UNEXPECTED OPERATOR")
  }

def combo(in:Int, computer: Computer): Long =
//  printDebug(s"    combo: ${in}")
  in match
    case n if 0 to 3 contains n => return n
    case 4 => return computer.regA
    case 5 => return computer.regB
    case 6 => return computer.regC
    case 7 => {
      println("Unexpected command")
      return Int.MaxValue
    }
    case _ => {
      println("Doubly Unexpected command")
      return Int.MaxValue
    }

inline def adv(in: Int, computer: Computer) : Unit =
//  printDebug(s"0 = adv, (${in} ${computer.pointer}) (${computer.regA}, ${computer.regB}, ${computer.regC})")
  val numerator: Long = computer.regA
  val divisor: Long = pow(2,combo(in, computer)).toLong
  computer.regA = numerator/divisor
  computer.pointer += 2

inline def bxl(in: Int, computer: Computer): Unit =
//  printDebug(s"1 = bxl, (${in} ${computer.pointer}) (${computer.regA}, ${computer.regB}, ${computer.regC})")
  computer.regB = computer.regB ^ in
  computer.pointer += 2

inline def bst(in: Int, computer: Computer): Unit =
//  printDebug(s"2 = bst, (${in} ${computer.pointer}) (${computer.regA}, ${computer.regB}, ${computer.regC})")
  computer.regB = combo(in, computer) % 8
  computer.pointer += 2

inline def jnz(in: Int, computer: Computer): Unit =
//  printDebug(s"3 = jnz, (${in} ${computer.pointer}) (${computer.regA}, ${computer.regB}, ${computer.regC})")
  if (computer.regA == 0) {
    computer.pointer += 2 // very poorly explained
  } else {
    computer.pointer = in
  }

inline def bxc(in: Int, computer: Computer): Unit =
//  printDebug(s"4 = bxc, (${in} ${computer.pointer}) (${computer.regA}, ${computer.regB}, ${computer.regC})")
  computer.regB = computer.regB ^ computer.regC
  computer.pointer += 2

inline def out(in: Int, computer: Computer): Unit =
//  printDebug(s"5 = out, (${in} ${computer.pointer}) (${computer.regA}, ${computer.regB}, ${computer.regC})")
  val value = combo(in, computer) % 8
  computer.output += value.toInt
  computer.pointer += 2

inline def bdv(in: Int, computer: Computer): Unit =
//  printDebug(s"6 = bdv, (${in} ${computer.pointer}) (${computer.regA}, ${computer.regB}, ${computer.regC})")
  val numerator: Long = computer.regA
  val divisor: Long = pow(2, combo(in, computer)).toInt
  computer.regB = numerator / divisor
  computer.pointer += 2

inline def cdv(in: Int, computer: Computer) : Unit =
//  printDebug(s"7 = cdv, (${in} ${computer.pointer}) (${computer.regA}, ${computer.regB}, ${computer.regC})")
  val numerator: Long = computer.regA
  val divisor: Long = pow(2,combo(in, computer)).toInt
  computer.regC = numerator/divisor
  computer.pointer += 2
