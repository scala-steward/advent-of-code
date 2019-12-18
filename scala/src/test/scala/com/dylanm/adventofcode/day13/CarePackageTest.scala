package com.dylanm.adventofcode.day13

import com.dylanm.adventofcode.day13.CarePackage.{numOfTileId, read}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CarePackageTest extends AnyFlatSpec with Matchers {
  private val filePath = "src/main/resources/day13/input.txt"

  behavior of "Care Package"

  it should "produce the correct answer for puzzle 1" in {
    numOfTileId(read(filePath), tileId = 2) shouldBe 268
  }
}
