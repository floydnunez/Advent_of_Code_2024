package day14

import scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

case class Robot(var x: Int, var y: Int, var vx: Int, var vy: Int)

@main def main(): Unit =
  val fileName = "inputs/day14/example.txt"
  var widthV = 101
  var heightV = 103
  if (fileName == "inputs/day14/example.txt") {
    widthV = 11
    heightV = 7
  }
  val width = widthV
  val height = heightV

  val fileContents = Source.fromFile(fileName).getLines().toArray

  println(fileContents)

  val listRobotsMut = ListBuffer[Robot]()

  fileContents.foreach{ line =>
    listRobotsMut += parseRobot(line)
  }

  val listRobots = listRobotsMut.toList

  println(s"${listRobots}")

def parseRobot(str: String): Robot =
  val parts = str.split(" ")
  val pdata = parts(0).split("=")
  val pos = pdata(1).split(",")
  val vdata = parts(1).split("=")
  val vel = vdata(1).split(",")
  Robot(x=pos(0).toInt, pos(1).toInt, vx = vel(0).toInt, vy = vel(1).toInt)