package com.dylanm.adventofcode.day09

import com.dylanm.adventofcode.utils.IntCode

import scala.io.Source

object SensorBoost {

  def puzzle1(filePath: String): List[Long] = runBoost(read(filePath), input = 1)

  def puzzle2(filePath: String): List[Long] = runBoost(read(filePath), input = 2)

  def read(filePath: String): List[Long] =
    Source.fromFile(filePath).getLines.toList.head.split(",")
      .map(_.toLong)
      .toList

  def runBoost(program: List[Long], input: Int): List[Long] = IntCode.runIntCode(program, input)

}