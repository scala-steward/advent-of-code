package com.dylanm.adventofcode.day09

import com.dylanm.adventofcode.utils.IntCode
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SensorBoostTest extends AnyFlatSpec with Matchers {
  private val filePath = "src/main/resources/day09/input.txt"

  behavior of "SensorBoost"

  it should "produce correct results running improved IntCode 1" in {
    val program = List[Long](109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99)
    IntCode.runIntCode(program, List()) shouldBe program
  }

  it should "produce correct results running improved IntCode 2" in {
    val program = List[Long](1102, 34915192, 34915192, 7, 4, 7, 99, 0)

    val result = IntCode.runIntCode(program, List())

    result shouldBe List(1219070632396864L)
    result.head.toString.length shouldBe 16
  }

  it should "produce correct results running improved IntCode 3" in {
    val inputValue = 1125899906842624L
    val program = List[Long](104, inputValue, 99)
    IntCode.runIntCode(program, List()) shouldBe List(inputValue)
  }

  it should "produce the correct result for puzzle 1" in {
    SensorBoost.puzzle1(filePath) shouldBe List(2350741403L)
  }

  it should "produce the correct result for puzzle 2" in {
    SensorBoost.puzzle2(filePath) shouldBe List(53088)
  }

}