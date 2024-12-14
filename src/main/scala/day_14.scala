package day14

import java.text.NumberFormat
import java.util.Locale
import scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.boundary
import scala.util.boundary.break

case class Robot(id: Int, var x: Int, var y: Int, var vx: Int, var vy: Int)

def generateRobots(fileContents: Array[String]): List[Robot] = {
  val listRobotsMut = ListBuffer[Robot]()
  var id = 0
  fileContents.foreach { line =>
    val robot = parseRobot(id, line)
    id += 1
    listRobotsMut += robot
  }
  val listRobots = listRobotsMut.toList
  listRobots
}
def calcSecurity(width: Int, height: Int, listRobots: List[Robot]) = {
  val q1 = countQuadrant(listRobots, 0, width / 2, 0, height / 2) //top left
  val q2 = countQuadrant(listRobots, width / 2 + 1, width, 0, height / 2) //top right
  val q3 = countQuadrant(listRobots, 0, width / 2, height / 2 + 1, height) //bottom left
  val q4 = countQuadrant(listRobots, width / 2 + 1, width, height / 2 + 1, height) //bottom right
  val coefp1 = q1 * q2 * q3 * q4
//  println(s"${q1} ${q2} ${q3} ${q4}")
  coefp1
}
@main def main(): Unit =
  val fileName = "inputs/day14/input.txt"
  var widthV = 101
  var heightV = 103
  if (fileName == "inputs/day14/example.txt") {
    widthV = 11
    heightV = 7
  }
  val width = widthV
  val height = heightV
  println(s"${width} x ${height}")

  val fileContents = Source.fromFile(fileName).getLines().toArray

  println(fileContents)
  var securityAt100 = 0L
  {
    val listRobots = generateRobots(fileContents)
    //  println(s"${listRobots}, ${listRobots.length} robots")

    //  printRobots(listRobots, width, height)

    for (turn <- 0 until 100) {
      listRobots.foreach { robot =>
        update(robot, width, height)
      }
    }

    val coefp1: Long = calcSecurity(width, height, listRobots)
    securityAt100 = coefp1
    println(s"Day 14 part 1: ${coefp1}") // 225521010
  }
  //part 2
  val listRobots = generateRobots(fileContents)
  var turn: Long = 0
  val maxTurns: Long = 10_000_000_000L
  val formatter = NumberFormat.getInstance(Locale.GERMANY)

  while (turn < maxTurns) {
    if (turn % 10_000_000 == 0) {
      println("10 million turns")
    }
    turn += 1
    listRobots.foreach { robot =>
      update(robot, width, height)
    }
    val coefsec: Long = calcSecurity(width, height, listRobots)
    var minCoef = securityAt100 * 0.4
    if (coefsec < minCoef ) {
      minCoef = coefsec
      val formattedNumber = formatter.format(minCoef)
      val formattedTurn = formatter.format(turn)
      println(s"tree at ${turn} (${formattedTurn}) security coefficient = ${formattedNumber}")
      printRobots(listRobots, width, height)
    }
  }
  //Part 2 7774

def checkTree(robots: List[Robot], width: Int, height: Int): Boolean =
  //I guess there shouldn't be any robots near the top corners
  val off = 29
  val cornerDistance = width/2 - off
  var noRobotInTopLeftCorner = true
  var noRobotInTopRightCorner = true
//  var noRobotInBottomLeftCorner = true
//  var noRobotInBottomRightCorner = true
  boundary {
    robots.foreach { robot =>
      if (robot.x < width / 2 - off && robot.y < height / 2 - off && robot.x + robot.y < cornerDistance) {
          noRobotInTopLeftCorner = false
          break()
      }
      if (robot.x > width / 2 + off && robot.y < height / 2 - off && (width - robot.x) + robot.y < cornerDistance) {
        noRobotInTopRightCorner = false
        break()
      }
//      if (robot.x < width / 2 - off && robot.y > height / 2 + off) {
//        noRobotInBottomLeftCorner = false
//        break()
//      }
//      if (robot.x > width / 2 + off && robot.y > height / 2 + off) {
//        noRobotInBottomRightCorner = false
//        break()
//      }
    }
  }
  noRobotInTopLeftCorner && noRobotInTopRightCorner
//  noRobotInTopLeftCorner && noRobotInTopRightCorner && noRobotInBottomLeftCorner && noRobotInBottomRightCorner

def printRobots(robots: List[Robot], width: Int, height: Int): Unit =
  println("")
  val listPrinted = ListBuffer[Int]()
  var str = ""
  var log = ""
  for(y <- 0 until height) {
    for(x <- 0 until width) {
      var howMany = 0
      robots.foreach{ robot =>
        if (robot.id == 2) {
          log += "2! "
        }
        if (robot.x == x && robot.y == y) {
          if (robot.id == 2) {
            log += s"found at ${x} ${y}"
          }
          howMany += 1
          listPrinted += robot.id
        }
      }
      if (howMany == 0) {
        str += "."
      } else {
        str += howMany.toString
      }
    }
    str += "\n"
  }
  println(str)
//  println(s"${robots}, ${robots.length} robots printed robots: ${listPrinted.toList} ${listPrinted.length}")
//  println(log)

def countQuadrant(robots: List[Robot], inix: Int, finx: Int, iniy: Int, finy: Int): Long =
//  println(s"counting robots between x = (${inix} to ${finx}) and y = (${iniy} to ${finy})")
  var result = 0L
  robots.foreach{ robot =>
    if (inix <= robot.x && finx > robot.x && iniy <= robot.y && finy > robot.y) {
      result += 1
//      println(s"robot: ${robot}")
    }
  }
  result

def update(robot: Robot, width: Int, height: Int): Unit =
  robot.x += robot.vx
  robot.y += robot.vy
  if (robot.x >= width) {
    robot.x -= width
  }
  if (robot.y >= height) {
    robot.y -= height
  }
  if (robot.x < 0) {
    robot.x += width
  }
  if (robot.y < 0) {
    robot.y += height
  }

def parseRobot(id: Int, str: String): Robot =
  val parts = str.split(" ")
  val pdata = parts(0).split("=")
  val pos = pdata(1).split(",")
  val vdata = parts(1).split("=")
  val vel = vdata(1).split(",")
  Robot(id=id, x=pos(0).toInt, pos(1).toInt, vx = vel(0).toInt, vy = vel(1).toInt)