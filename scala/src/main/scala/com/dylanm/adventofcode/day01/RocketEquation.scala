package com.dylanm.adventofcode.day01

import scala.annotation.tailrec
import scala.io.Source

/**
 * The Tyranny of the Rocket Equation
 */
object RocketEquation {

  def read(filePath: String): List[Int] = Source.fromFile(filePath).getLines.map(_.toInt).toList

  def puzzle1(masses: List[Int]): Long = calculateSumOfFuelRequirements(masses, simpleFuelRequired)

  def puzzle2(masses: List[Int]): Long = calculateSumOfFuelRequirements(masses, recursiveFuelRequired)

  def recursiveFuelRequired(mass: Int): Long = {
    @tailrec
    def helper(acc: Long, previousAmount: Int): Long = {
      val next = simpleFuelRequired(previousAmount)
      if (next <= 0) acc
      else helper(acc + next, next)
    }

    helper(0L, mass)
  }

  def simpleFuelRequired(mass: Int): Int = (mass / 3) - 2

  def calculateSumOfFuelRequirements(masses: List[Int],
                                     fuelRequired: (Int) => Long): Long =
    masses.foldLeft(0L)((acc, mass) => acc + fuelRequired(mass))
}
