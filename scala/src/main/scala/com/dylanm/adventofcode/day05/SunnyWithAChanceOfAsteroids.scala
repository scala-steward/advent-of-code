package com.dylanm.adventofcode.day05

import scala.annotation.tailrec
import scala.io.Source

sealed trait ParameterMode

case object PositionMode extends ParameterMode

case object ImmediateMode extends ParameterMode

object SunnyWithAChanceOfAsteroids {

  def puzzle1(filePath: String): List[Int] = run(read(filePath), inputInstruction = 1)

  def read(filePath: String): List[Int] =
    Source.fromFile(filePath).getLines.toList.head.split(",")
      .map(_.toInt)
      .toList

  def run(data: List[Int], inputInstruction: Int): List[Int] = {
    def calculateNewIndexAndValueOps1And2(seq: IndexedSeq[Int],
                                          i: Int,
                                          op: (Int, Int) => Int,
                                          modes: (ParameterMode, ParameterMode)): (Int, Int) = {
      val firstValue = modes._1 match {
        case PositionMode => seq(seq(i + 1))

        case ImmediateMode => seq(i + 1)
      }

      val secondValue = modes._2 match {
        case PositionMode => seq(seq(i + 2))

        case ImmediateMode => seq(i + 2)
      }

      val destinationIndex = seq(i + 3)

      (destinationIndex, op(firstValue, secondValue))
    }

    @tailrec
    def helper(latest: IndexedSeq[Int],
               currIndex: Int,
               inputInstruction: Int,
               outputs: List[Int]): List[Int] = {
      if (currIndex > latest.size) outputs
      else {
        val (opCode, mode1, mode2, _) = parseOpCodeAndModes(latest(currIndex))

        opCode match {
          case 1 =>
            val (destinationIndex, destinationValue) = calculateNewIndexAndValueOps1And2(latest, currIndex, _ + _, (mode1, mode2))
            helper(latest.updated(destinationIndex, destinationValue),
              currIndex + 4,
              inputInstruction = latest(currIndex + 4),
              outputs)

          case 2 =>
            val (destinationIndex, destinationValue) = calculateNewIndexAndValueOps1And2(latest, currIndex, _ * _, (mode1, mode2))
            helper(
              latest.updated(destinationIndex, destinationValue),
              currIndex + 4,
              inputInstruction = latest(currIndex + 4),
              outputs)

          /*
          Opcode 3 takes a single integer as input and saves it to the position given by its only parameter.
          For example, the instruction 3,50 would take an input value and store it at address 50.
           */
          case 3 => helper(latest.updated(latest(currIndex + 1), inputInstruction), currIndex + 2, inputInstruction = latest(currIndex + 2), outputs)

          /*
          Opcode 4 outputs the value of its only parameter.
          For example, the instruction 4,50 would output the value at address 50.
           */
          case 4 =>
            val newOutput = mode1 match {
              case PositionMode => latest(latest(currIndex + 1))
              case ImmediateMode => latest(currIndex + 1)
            }
            helper(latest, currIndex + 2, inputInstruction = latest(currIndex + 2), newOutput :: outputs)

          case 99 => outputs
        }
      }
    }

    helper(data.toIndexedSeq, currIndex = 0, inputInstruction, List())
  }

  def parseOpCodeAndModes(value: Int): (Int, ParameterMode, ParameterMode, ParameterMode) = {
    val opCode = value % 100

    def mapToMode(i: Int): ParameterMode =
      i match {
        case 0 => PositionMode
        case 1 => ImmediateMode
      }

    val mode1 = value / 100 % 10
    val mode2 = value / 1000 % 10
    val mode3 = value / 10000 % 10

    (opCode, mapToMode(mode1), mapToMode(mode2), mapToMode(mode3))
  }

}
