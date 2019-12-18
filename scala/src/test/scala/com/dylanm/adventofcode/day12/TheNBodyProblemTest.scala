package com.dylanm.adventofcode.day12

import com.dylanm.adventofcode.day12.TheNBodyProblem.{numIterationsToGetBackToPrevious,
  read, totalEndEnergy, lowestCommonMultiple}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TheNBodyProblemTest extends AnyFlatSpec with Matchers {
  private val filePath = "src/main/resources/day12/input.txt"

  behavior of "The N-Body Problem"

  it should "read data correctly" in {
    read(filePath) shouldBe List(
      Position(x = 15, y = -2, z = -6),
      Position(x = -5, y = -4, z = -11),
      Position(x = 0, y = -6, z = 0),
      Position(x = 5, y = 9, z = 6))
  }

  it should "correctly calculate energies" in {
    val posAndVel = PositionAndVelocity(
      Position(x = -29, y = 11, z = -1),
      Velocity(x = -3, y = 7, z = 4))

    posAndVel.kineticEnergy shouldBe 14
    posAndVel.potentialEnergy shouldBe 41
    posAndVel.totalEnergy shouldBe 574
  }

  it should "correctly total energy after multiple iterations" in {
    val startingPositions = List(
      Position(x = -8, y = -10, z = 0),
      Position(x = 5, y = 5, z = 10),
      Position(x = 2, y = -7, z = 3),
      Position(x = 9, y = -8, z = -3))

    totalEndEnergy(startingPositions, numIterations = 100) shouldBe 1940
  }

  it should "produce correct solution for puzzle 1" in {
    val startingPositions = read(filePath)

    totalEndEnergy(startingPositions, numIterations = 1000) shouldBe 6735
  }

  it should "calculate the lowest common multiple" in {
    lowestCommonMultiple(15, 20) shouldBe 60
  }

  it should "calculate the number of steps taken to get back to previously seen state 1" in {
    val startingPositions = List(
      Position(x = -1, y = 0, z = 2),
      Position(x = 2, y = -10, z = -7),
      Position(x = 4, y = -8, z = 8),
      Position(x = 3, y = 5, z = -1))

    numIterationsToGetBackToPrevious(startingPositions) shouldBe 2772
  }

  it should "calculate the number of steps taken to get back to previously seen state 2" in {
    val startingPositions = List(
      Position(x = -8, y = -10, z = 0),
      Position(x = 5, y = 5, z = 10),
      Position(x = 2, y = -7, z = 3),
      Position(x = 9, y = -8, z = -3))

    numIterationsToGetBackToPrevious(startingPositions) shouldBe 4686774924L
  }

  it should "produce the correct solution for puzzle 2" in {
    val startingPositions = read(filePath)

    numIterationsToGetBackToPrevious(startingPositions) shouldBe 326489627728984L
  }

}