package com.dylanm.adventofcode.day06

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

case class Orbit(orbiter: String, orbitee: String)

object UniversalOrbitMap {
  private val you = "YOU"
  private val santa = "SAN"
  private val centroid = "COM"

  def puzzle1(filePath: String): Int = totalNumOrbits(read(filePath))

  def totalNumOrbits(orbits: List[Orbit]): Int = {
    val orbiterToOrbitee = orbits.map(orbit => orbit.orbiter -> orbit.orbitee).toMap

    val orbiters = orbits.map(_.orbiter).toSet
    val orbitees = orbits.map(_.orbitee).toSet

    val centroids = orbitees.filter(!orbiters.contains(_))

    val memo: mutable.HashMap[String, Int] = new mutable.HashMap()

    def findNumOrbits(orbiter: String): Int =
      if (centroids.contains(orbiter)) 0
      else memo.getOrElseUpdate(orbiter, 1 + findNumOrbits(orbiterToOrbitee(orbiter)))

    @tailrec
    def findTotalNumOrbits(remainingOrbiters: Set[String],
                           count: Int = 0): Int = {
      if (remainingOrbiters.isEmpty) count
      else {
        val newOrbiter = remainingOrbiters.head
        val numOrbits = findNumOrbits(newOrbiter)
        findTotalNumOrbits(remainingOrbiters.tail, count + numOrbits)
      }
    }

    findTotalNumOrbits(orbiters)
  }

  def read(filePath: String): List[Orbit] =
    Source.fromFile(filePath).getLines.toList
      .map(line => line.split("\\)"))
      .map(splits => Orbit(splits.tail.head, splits.head))

  def puzzle2(filePath: String): Int = minOrbitalTransfersToSanta(read(filePath))

  def minOrbitalTransfersToSanta(orbits: List[Orbit]): Int = {
    val orbiterToOrbitee = orbits.map(orbit => orbit.orbiter -> orbit.orbitee).toMap

    @tailrec
    def pathToCentroid(orbiter: String, path: List[String] = List()): List[String] =
      if (orbiter == centroid) path
      else pathToCentroid(orbiterToOrbitee(orbiter), orbiter :: path)

    @tailrec
    def findDivergentPaths(rem1: List[String], rem2: List[String]): (List[String], List[String]) =
      (rem1, rem2) match {
        case (head1 :: tail1, head2 :: tail2) if head1 == head2 => findDivergentPaths(tail1, tail2)
        case _ => (rem1, rem2)
      }

    val (div1, div2) = findDivergentPaths(pathToCentroid(you), pathToCentroid(santa))

    (div1.size - 1) + (div2.size - 1)
  }
}