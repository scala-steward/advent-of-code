package com.dylanm.adventofcode.day13

import com.dylanm.adventofcode.utils.{IntCode, State}

import scala.annotation.tailrec
import scala.io.Source

case class Tile(x: Int, y: Int, id: Int)

object CarePackage {

  def read(filePath: String): List[Long] =
    Source.fromFile(filePath).getLines.toList.head.split(",")
      .map(_.toLong)
      .toList

  def generateTiles(program: List[Long]): Set[Tile] = {
    @tailrec
    def tiles(currentState: State,
              currentTiles: Set[Tile] = Set()): Set[Tile] =
      IntCode.runUntilOutput(currentState) match {
        case None => currentTiles

        case Some(newState) if newState.outputs.size % 3 == 0 =>
          val newTile = Tile(newState.outputs.tail.tail.head.toInt, newState
            .outputs.tail.head.toInt,
            newState.outputs.head.toInt)
          tiles(newState, currentTiles + newTile)

        case Some(newState) => tiles(newState, currentTiles)
      }

    val indexToValue = program.zipWithIndex.map { case (value, idx) => (idx, value) }.toMap.withDefaultValue(0L)
    val initialState = State(indexToValue, List(0))

    tiles(initialState)
  }

  def numOfTileId(program: List[Long], tileId: Int): Int = generateTiles(program).count(_.id == tileId)
}
