package com.dylanm.adventofcode.day03

import scala.annotation.tailrec
import scala.io.Source

case class DirectionAndDistance(direction: Char, distance: Int)

case class Point(x: Int, y: Int) {
  def left(distance: Int): Point = copy(x = x - distance)

  def right(distance: Int): Point = copy(x = x + distance)

  def up(distance: Int): Point = copy(y = y + distance)

  def down(distance: Int): Point = copy(y = y - distance)
}

object CrossedWires {
  def puzzle1(filePath: String): Int = {
    val (wire1Path, wire2Path) = read(filePath)
    manhattanDistanceToNearestIntersection(wire1Path, wire2Path)
  }

  def puzzle2(filePath: String): Int = {
    val (wire1Path, wire2Path) = read(filePath)
    minWireLengthToIntersection(wire1Path, wire2Path)
  }

  def minWireLengthToIntersection(wire1Path: List[DirectionAndDistance],
                                  wire2Path: List[DirectionAndDistance]): Int = {
    val seenPointsAndWireLength1 = genSeenPointsAndWireLength(wire1Path)
    val seenPointsAndWireLength2 = genSeenPointsAndWireLength(wire2Path)

    val intersectionPoints = seenPointsAndWireLength1.keySet.intersect(seenPointsAndWireLength2.keySet)

    intersectionPoints.foldLeft(Integer.MAX_VALUE) {
      (minSoFar, point) => {
        val totalWireLength = seenPointsAndWireLength1(point) + seenPointsAndWireLength2(point)
        if (totalWireLength < minSoFar) totalWireLength else minSoFar
      }
    }
  }

  def read(filePath: String): (List[DirectionAndDistance], List[DirectionAndDistance]) = {
    val lines = Source.fromFile(filePath).getLines.toList.map(line =>
      line.split(",")
        .map { dir => DirectionAndDistance(dir.head, dir.tail.toInt) }
        .toList
    )

    (lines.head, lines(1))
  }

  def manhattanDistanceToNearestIntersection(wire1Path: List[DirectionAndDistance],
                                             wire2Path: List[DirectionAndDistance]): Int = {
    val seenPoints1 = genSeenPointsAndWireLength(wire1Path).keySet
    val seenPoints2 = genSeenPointsAndWireLength(wire2Path).keySet

    val intersections = seenPoints1.intersect(seenPoints2)

    val manhattanDistances = intersections.toList.map { case Point(x, y) => Math.abs(x) + Math.abs(y) }

    manhattanDistances.min
  }

  @tailrec
  private def genSeenPointsAndWireLength(remainingDirections: List[DirectionAndDistance],
                                         currentPoint: Point = Point(0, 0),
                                         currentLength: Int = 0,
                                         seenPointsAndLengths: Map[Point, Int] = Map()): Map[Point, Int] = {

    remainingDirections match {
      case Nil => seenPointsAndLengths
      case dirs => {
        val nextDir = dirs.head

        val distance = nextDir.distance

        val (newSeenPointsAndLengths, newCurrentPoint) =
          nextDir.direction match {
            case 'U' => ((1 to distance).map(a => currentPoint.up(a) -> (currentLength + a)).toMap,
              currentPoint.up(distance))

            case 'D' => ((1 to distance).map(a => currentPoint.down(a) -> (currentLength + a)).toMap,
              currentPoint.down(distance))

            case 'L' => ((1 to distance).map(a => currentPoint.left(a) -> (currentLength + a)).toMap,
              currentPoint.left(distance))

            case 'R' => ((1 to distance).map(a => currentPoint.right(a) -> (currentLength + a)).toMap,
              currentPoint.right(distance))
          }

        val allSeenAndLengths = newSeenPointsAndLengths.foldLeft(seenPointsAndLengths) {
          case (acc, (point, distance)) =>
            if (acc.contains(point)) acc
            else acc + ((point, distance))
        }

        genSeenPointsAndWireLength(remainingDirections.tail,
          newCurrentPoint,
          currentLength + distance,
          allSeenAndLengths)
      }
    }
  }

}
