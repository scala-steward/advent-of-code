package com.dylanm.adventofcode.day11

import com.dylanm.adventofcode.utils.{IntCode, State}

import scala.annotation.tailrec
import scala.io.Source

case class Coordinate(x: Int, y: Int) {
  def move(direction: Direction): Coordinate = {
    direction match {
      case Up => copy(y = y + 1)
      case Down => copy(y = y - 1)
      case Right => copy(x = x + 1)
      case Left => copy(x = x - 1)
    }
  }
}

object SpacePolice {
  def read(filePath: String): List[Long] =
    Source.fromFile(filePath).getLines.toList.head.split(",")
      .map(_.toLong)
      .toList

  def numSquaresPainted(program: List[Long]): Int = {
    val startingPoint = Coordinate(0, 0)

    @tailrec
    def rec(currentState: State,
            currentPosition: Coordinate,
            currentDirection: Direction,
            paintedAtSomePoint: Set[Coordinate] = Set(),
            currentlyWhite: Set[Coordinate] = Set()): Int = {
      IntCode.runUntilOutput(currentState) match {
        case None => paintedAtSomePoint.size
        case Some(newState) if newState.outputs.size % 2 == 0 => {
          val (directionToTurn, colourToPaint) = (newState.outputs.head, newState.outputs.tail.head)

          val (newWhiteOnes, newPaintedAtSomePoint) =
            colourToPaint match {
              case 0 => // paint black
                if (currentlyWhite.contains(currentPosition)) (currentlyWhite - currentPosition, paintedAtSomePoint + currentPosition)
                else (currentlyWhite, paintedAtSomePoint)
              case 1 => // paint white
                if (!currentlyWhite.contains(currentPosition)) (currentlyWhite + currentPosition, paintedAtSomePoint + currentPosition)
                else (currentlyWhite, paintedAtSomePoint)
            }

          val newDirection = if (directionToTurn == 0) currentDirection.counterClockwise else
            currentDirection.clockwise
          val newPosition = currentPosition.move(newDirection)
          val newInput = if (currentlyWhite.contains(newPosition)) 1 else 0

          rec(newState.copy(inputInstructions = List(newInput)), newPosition, newDirection, newPaintedAtSomePoint, newWhiteOnes)
        }
        case Some(newState) => rec(newState, currentPosition, currentDirection, paintedAtSomePoint, currentlyWhite)
      }
    }

    val indexToValue = program.zipWithIndex.map { case (value, idx) => (idx, value) }.toMap
      .withDefaultValue(0L)
    val initialState = State(indexToValue, List(0), List(), 0, 0)

    rec(initialState, startingPoint, Up)
  }
}