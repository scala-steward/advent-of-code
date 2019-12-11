package com.dylanm.adventofcode.day06

import com.dylanm.adventofcode.day06.UniversalOrbitMap._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UniversalOrbitMapTest extends AnyFlatSpec with Matchers {

  private val filePath = "src/main/resources/day06/input.txt"

  behavior of "UniversalOrbitMap"

  it should "produce correct result for puzzle 1" in {
    puzzle1(filePath) shouldBe 194721
  }

  it should "produce correct result for puzzle 2" in {
    puzzle2(filePath) shouldBe 316
  }

  it should "produce correct total number of orbits" in {
    val orbits = List(
      Orbit("B", "COM"),
      Orbit("C", "B"),
      Orbit("D", "C"),
      Orbit("E", "D"),
      Orbit("F", "E"),
      Orbit("G", "B"),
      Orbit("H", "G"),
      Orbit("I", "D"),
      Orbit("J", "E"),
      Orbit("K", "J"),
      Orbit("L", "K"))

    totalNumOrbits(orbits) shouldBe 42
  }

  it should "produce correct min orbital transfers to Santa" in {
    val orbits = List(
      Orbit("B", "COM"),
      Orbit("C", "B"),
      Orbit("D", "C"),
      Orbit("E", "D"),
      Orbit("F", "E"),
      Orbit("G", "B"),
      Orbit("H", "G"),
      Orbit("I", "D"),
      Orbit("J", "E"),
      Orbit("K", "J"),
      Orbit("L", "K"),
      Orbit("YOU", "K"),
      Orbit("SAN", "I"))

    minOrbitalTransfersToSanta(orbits) shouldBe 4
  }

}