package day12

import day07.Oper.{add, mul}

import java.text.NumberFormat
import java.util.Locale
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.util.boundary
import scala.util.boundary.break
import scala.util.matching.Regex

case class Square(c: String, var visited: Boolean)
case class Seg(var x1: Int, var y1: Int, var x2: Int, var y2: Int, horizontal: Boolean, name: String)
case class Parcel(terrain: List[Square], perimeter: Int, var segments: ListBuffer[Seg])

@main def main(): Unit =
  val fileName = "inputs/day12/input.txt"
  val fileContents = Source.fromFile(fileName).getLines().toArray

  printMap(fileContents)
  val width = fileContents(0).length
  val height = fileContents.length

  val map = fileToMap(fileContents)
  println(map)
  val allTerrains = divideParcels(map, width, height)
  println(s"\nparcels: ${allTerrains}")
  val total_1_area = allTerrains.map(parcel => parcel.terrain.length * parcel.perimeter).sum

  println(s"Day 12 part 1: ${total_1_area}") // 1446042

  var total_2_area = 0
  allTerrains.foreach{ parcel =>
    joinSegments(parcel)
    total_2_area += parcel.segments.length * parcel.terrain.length
  }
  printParcels(allTerrains)
  println(s"Day 12 part 2: ${total_2_area}") // 902742

def printParcels(parcels: List[Parcel]): Unit =
  parcels.foreach{ parcel =>
    println(s"${parcel.terrain(0).c} area: ${parcel.terrain.length} sides: ${parcel.segments.length} = ${parcel.segments}")
  }

def joinSegments(parcel: Parcel): Unit =
  val list = recursiveJoin(parcel.segments)
  parcel.segments = list

def recursiveJoin(segs: ListBuffer[Seg]): ListBuffer[Seg] = {
  var didItJoin = false
  boundary {
    segs.foreach { seg1 =>
      segs.foreach { seg2 =>
        if (seg1 != seg2 && seg1.name == seg2.name) { //they are different AND they face the same side.
          if (seg1.horizontal) { // both horizontal
            if (seg1.y1 == seg2.y1) { // same height
              if (seg1.x1 == seg2.x2) { //seg2 before seg1
                seg1.x1 = seg2.x1 // widen it to the left
                segs -= seg2 // remove from the list
                didItJoin = true // mark as joined!
                break() // break to avoid handling iteration problems when removing from the list
              } else if(seg1.x2 == seg2.x1) { //seg1 before seg2
                seg1.x2 = seg2.x2 //widen it to the right
                segs -= seg2
                didItJoin = true
                break()
              }
            }
          } else if (!seg1.horizontal) { //both vertical
            if (seg1.x1 == seg2.x1) { // same alignment
              if (seg1.y1 == seg2.y2) { //seg2 above seg1
                seg1.y1 = seg2.y1 // heighten it upwards
                segs -= seg2
                didItJoin = true
                break()
              } else if(seg1.y2 == seg2.y1) { //seg1 above seg2
                seg1.y2 = seg2.y2 // lower it
                segs -= seg2
                didItJoin = true
                break()
              }
            }
          }
        }
      }
    }
  }
  if (didItJoin) { // if we fucked around with the sequence, just call it again recursively. It' fine.
    return recursiveJoin(segs)
  }
  return segs //otherwise...we are done!
}

def divideParcels(map: Map[(Int, Int), Square], width: Int, height: Int): List[Parcel] =
  val result = ListBuffer[Parcel]()
  for(yy <- 0 until height) {
    for(xx <- 0 until width) {
      val sq = map.get(xx, yy)
      sq match
        case None => println("Cuadrado vacio??")
        case Some(square) =>
          if (!square.visited) {
            formParcelFrom(map, xx, yy).foreach( parcel => result += parcel )
          }
    }
  }
  result.toList

def formParcelFrom(map: Map[(Int, Int), Square], x: Int, y: Int): Option[Parcel] = {
  val list = ListBuffer[Square]()
  val listSegments = ListBuffer[Seg]()
  var perimeter = 0
  map.get((x, y)).map { currSquare =>
    if (currSquare.visited) {
      return None
    }
    currSquare.visited = true
    list += currSquare
    val directions = List((x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)) //up, right, down, left
    val nextSquares = ListBuffer[Square]()
    directions.foreach { dir =>
      val ns = map.get(dir)
      ns match {
        case None =>
          perimeter += 1
          listSegments += calcSegment(x, y, dir._1, dir._2)
        case Some(nextSq) =>
          if (nextSq.c != currSquare.c) {
            perimeter += 1
            listSegments += calcSegment(x, y, dir._1, dir._2)
          }
          else if (!nextSq.visited) {
            val subparcel = formParcelFrom(map, dir._1, dir._2)
            subparcel.foreach { sp =>
              list ++= sp.terrain
              listSegments ++= sp.segments
              perimeter += sp.perimeter
            }
          }
      }
    }
  }
  return Some(Parcel(terrain = list.toList, perimeter = perimeter, segments = listSegments))
}

def calcSegment(x1: Int, y1: Int, x2: Int, y2: Int): Seg = {
  if (y1 > y2) { //up
    return Seg(x1 = x1, y1 = y1, x2 = x1+1, y2=y1, true, "up")
  } else if (y2 > y1) { //down
    return Seg(x1 = x1, y1 = y2, x2 = x1+1, y2=y2, true, "down")
  } else if (x1 > x2) { //left
    return Seg(x1 = x1, y1 = y1, x2 = x1, y2=y1 + 1, false, "left")
  } else { //(x1 < x2) right
    return Seg(x1 = x1+1, y1 = y1, x2 = x1+1, y2=y1 + 1, false, "right")
  }
}

    def printMap(strings: Array[String]):Unit = {
  strings.foreach( line => println(line) )
}

    def fileToMap(strings: Array[String]): Map[(Int, Int), Square] = {
  val map = mutable.Map[(Int, Int), Square]()
  strings.zipWithIndex.foreach { (line, yy) =>
    val chars = line.split("")
    chars.zipWithIndex.foreach { (letter, xx) =>
      map((xx, yy)) = Square(c = letter, visited = false)
    }
  }
  map.toMap
}