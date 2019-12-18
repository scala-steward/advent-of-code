package com.dylanm.adventofcode.day11

import com.dylanm.adventofcode.day11.SpacePolice._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SpacePoliceTest extends AnyFlatSpec with Matchers {
  private val filePath = "src/main/resources/day11/input.txt"
  private val program = read(filePath)

  behavior of "Space Police"

  it should "run puzzle 1" in {
    numSquaresPainted(program) shouldBe 2720
  }

  it should "print solution to puzzle 2" in {
    printCode(program)
  }

}