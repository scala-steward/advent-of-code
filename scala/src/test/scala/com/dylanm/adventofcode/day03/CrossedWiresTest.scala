package com.dylanm.adventofcode.day03

import CrossedWires.{manhattanDistanceToNearestIntersection, minWireLengthToIntersection}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CrossedWiresTest extends AnyFlatSpec with Matchers {

  behavior of "Crossed Wires"

  private val filePath = "src/main/resources/day03/input.txt"

  it should "produce the correct Manhattan distance 1" in {
    val wire1Path = List(DirectionAndDistance('R', 8),
      DirectionAndDistance('U', 5),
      DirectionAndDistance('L', 5),
      DirectionAndDistance('D', 3))

    val wire2Path = List(DirectionAndDistance('U', 7),
      DirectionAndDistance('R', 6),
      DirectionAndDistance('D', 4),
      DirectionAndDistance('L', 4))

    manhattanDistanceToNearestIntersection(wire1Path, wire2Path) shouldBe 6
  }

  it should "produce the correct Manhattan distance 2" in {
    val wire1Path = List(DirectionAndDistance('R', 75),
      DirectionAndDistance('D', 30),
      DirectionAndDistance('R', 83),
      DirectionAndDistance('U', 83),
      DirectionAndDistance('L', 12),
      DirectionAndDistance('D', 49),
      DirectionAndDistance('R', 71),
      DirectionAndDistance('U', 7),
      DirectionAndDistance('L', 72))

    val wire2Path = List(DirectionAndDistance('U', 62),
      DirectionAndDistance('R', 66),
      DirectionAndDistance('U', 55),
      DirectionAndDistance('R', 34),
      DirectionAndDistance('D', 71),
      DirectionAndDistance('R', 55),
      DirectionAndDistance('D', 58),
      DirectionAndDistance('R', 83))

    manhattanDistanceToNearestIntersection(wire1Path, wire2Path) shouldBe 159
  }

  it should "produce the correct Manhattan distance 3" in {
    val wire1Path = List(DirectionAndDistance('R', 98),
      DirectionAndDistance('U', 47),
      DirectionAndDistance('R', 26),
      DirectionAndDistance('D', 63),
      DirectionAndDistance('R', 33),
      DirectionAndDistance('U', 87),
      DirectionAndDistance('L', 62),
      DirectionAndDistance('D', 20),
      DirectionAndDistance('R', 33),
      DirectionAndDistance('U', 53),
      DirectionAndDistance('R', 51))

    val wire2Path = List(DirectionAndDistance('U', 98),
      DirectionAndDistance('R', 91),
      DirectionAndDistance('D', 20),
      DirectionAndDistance('R', 16),
      DirectionAndDistance('D', 67),
      DirectionAndDistance('R', 40),
      DirectionAndDistance('U', 7),
      DirectionAndDistance('R', 15),
      DirectionAndDistance('U', 6),
      DirectionAndDistance('R', 7))

    manhattanDistanceToNearestIntersection(wire1Path, wire2Path) shouldBe 135
  }

  it should "produce the correct result for puzzle 1" in {
    CrossedWires.puzzle1(filePath) shouldBe 266
  }

  it should "produce the correct result for puzzle 2" in {
    CrossedWires.puzzle2(filePath) shouldBe 19242
  }

  it should "produce the correct minimum total wire length to an intersection 1" in {
    val wire1Path = List(DirectionAndDistance('R', 75),
      DirectionAndDistance('D', 30),
      DirectionAndDistance('R', 83),
      DirectionAndDistance('U', 83),
      DirectionAndDistance('L', 12),
      DirectionAndDistance('D', 49),
      DirectionAndDistance('R', 71),
      DirectionAndDistance('U', 7),
      DirectionAndDistance('L', 72))

    val wire2Path = List(DirectionAndDistance('U', 62),
      DirectionAndDistance('R', 66),
      DirectionAndDistance('U', 55),
      DirectionAndDistance('R', 34),
      DirectionAndDistance('D', 71),
      DirectionAndDistance('R', 55),
      DirectionAndDistance('D', 58),
      DirectionAndDistance('R', 83))

    minWireLengthToIntersection(wire1Path, wire2Path) shouldBe 610
  }

  it should "produce the correct minimum total wire length to an intersection 2" in {
    val wire1Path = List(DirectionAndDistance('R', 98),
      DirectionAndDistance('U', 47),
      DirectionAndDistance('R', 26),
      DirectionAndDistance('D', 63),
      DirectionAndDistance('R', 33),
      DirectionAndDistance('U', 87),
      DirectionAndDistance('L', 62),
      DirectionAndDistance('D', 20),
      DirectionAndDistance('R', 33),
      DirectionAndDistance('U', 53),
      DirectionAndDistance('D', 51))

    val wire2Path = List(DirectionAndDistance('U', 98),
      DirectionAndDistance('R', 91),
      DirectionAndDistance('D', 20),
      DirectionAndDistance('R', 16),
      DirectionAndDistance('D', 67),
      DirectionAndDistance('R', 40),
      DirectionAndDistance('U', 7),
      DirectionAndDistance('R', 15),
      DirectionAndDistance('U', 6),
      DirectionAndDistance('R', 7))

    minWireLengthToIntersection(wire1Path, wire2Path) shouldBe 410
  }
}
