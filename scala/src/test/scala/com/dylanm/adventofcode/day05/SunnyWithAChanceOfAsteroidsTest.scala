package com.dylanm.adventofcode.day05

import com.dylanm.adventofcode.day05.SunnyWithAChanceOfAsteroids.{parseOpCodeAndModes, puzzle1}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SunnyWithAChanceOfAsteroidsTest extends AnyFlatSpec with Matchers {

  private val filePath = "src/main/resources/day05/input.txt"

  behavior of "SunnyWithAChanceOfAsteroids"

  it should "parse Op Codes and Modes" in {
    parseOpCodeAndModes(1002) shouldBe(2, PositionMode, ImmediateMode, PositionMode)
    parseOpCodeAndModes(10104) shouldBe(4, ImmediateMode, PositionMode, ImmediateMode)
  }

  it should "produce correct result for puzzle 1" in {
    val head :: tail = puzzle1(filePath)

    head shouldBe 9938601
    tail.forall(_ == 0) shouldBe true
  }

}
