package com.dylanm.adventofcode.day10

import com.dylanm.adventofcode.day10.MonitoringStation._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MonitoringStationTest extends AnyFlatSpec with Matchers {
  private val testResourcesDir = "src/test/resources/day10/"
  private val filePath = "src/main/resources/day10/input.txt"

  behavior of "Monitoring Station"

  it should "produce the correct result for puzzle 1" in {
    val asteroidMap = read(filePath)
    val (coordinate, numAsteroidsDetected) = findBestMonitoringStation(asteroidMap)

    (coordinate, numAsteroidsDetected) shouldBe(Coordinate(27, 19), 314)
  }

  it should "produce the correct result for puzzle 2" in {
    val asteroidMap = read(filePath)
    val monitoringStation = Coordinate(27, 19)

    val asteroidToBeVaporized200 = findNthAsteroidToBeVaporized(asteroidMap, monitoringStation, n = 200)

    asteroidToBeVaporized200 shouldBe Coordinate(15, 13)
    asteroidToBeVaporized200.x * 100 + asteroidToBeVaporized200.y shouldBe 1513
  }
}