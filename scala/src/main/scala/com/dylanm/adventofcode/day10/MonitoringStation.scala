package com.dylanm.adventofcode.day10

import scala.annotation.tailrec
import scala.io.Source

case class Coordinate(x: Int, y: Int) {
  def angleTo(other: Coordinate): Double = Math.atan2(other.x - x, other.y - y)

  def manhattanDistanceTo(other: Coordinate): Double = (other.x - x).abs + (other.y - y).abs
}

object MonitoringStation {
  def read(filePath: String): Map[Coordinate, Boolean] =
    Source.fromFile(filePath).getLines.toList.zipWithIndex.flatMap {
      case (chars, y) =>
        chars.toList.zipWithIndex.map {
          case ('.', x) => Coordinate(x, y) -> false
          case ('#', x) => Coordinate(x, y) -> true
        }
    }.toMap

  def findBestMonitoringStation(asteroidMap: Map[Coordinate, Boolean]): (Coordinate, Int) = {
    val asteroids = asteroidMap
      .filter(_._2)
      .keySet

    asteroids.foldLeft((Coordinate(0, 0), Integer.MIN_VALUE)) {
      case ((bestCoord, maxAsteroids), newCoord) =>
        val numFromNewCoord = findNumAsteroidsFromCoord(asteroids, newCoord)
        if (numFromNewCoord > maxAsteroids) (newCoord, numFromNewCoord)
        else (bestCoord, maxAsteroids)
    }
  }

  def findNumAsteroidsFromCoord(asteroids: Set[Coordinate], coordinate: Coordinate): Int = {
    val otherAsteroids = asteroids - coordinate
    otherAsteroids.map(coordinate.angleTo).size
  }

  def findNthAsteroidToBeVaporized(asteroidMap: Map[Coordinate, Boolean],
                                   monitoringStation: Coordinate,
                                   n: Int): Coordinate = {

    @tailrec
    def rec(remainingAsteroids: List[(Double, List[Coordinate])],
            vaporizedSoFar: List[Coordinate] = List()): Coordinate = {
      if (vaporizedSoFar.size == n || remainingAsteroids.isEmpty) vaporizedSoFar.head
      else {
        val nextCoordinates = remainingAsteroids.head._2

        nextCoordinates match {
          case head :: Nil => rec(remainingAsteroids.tail, head :: vaporizedSoFar)
          case head :: tail => rec(remainingAsteroids.tail :+ (remainingAsteroids.head._1, tail), head :: vaporizedSoFar)
        }
      }
    }

    val angleToAsteroids = asteroidMap.filter(_._2)
      .keySet
      .groupBy(monitoringStation.angleTo)
      .view.mapValues(a => a.toList.sortBy(monitoringStation.manhattanDistanceTo))
      .toList
      .sortBy(_._1)
      .reverse

    rec(angleToAsteroids)
  }
}