package com.dylanm.adventofcode.utils

import scala.annotation.tailrec

case class State(state: Map[Int, Long],
                 inputInstructions: List[Long],
                 outputs: List[Long],
                 pointer: Int,
                 relativeBase: Int)

object IntCode {
  def runIntCode(data: List[Long], inputInstruction: Long): List[Long] = runIntCode(data, List(inputInstruction))

  def runIntCode(data: List[Long], inputInstructions: List[Long]): List[Long] = {
    val indexToValue = data.zipWithIndex.map { case (value, idx) => (idx, value) }.toMap
      .withDefaultValue(0L)

    val initialState = State(indexToValue, inputInstructions, List(), 0, 0)

    @tailrec
    def rec(state: State): List[Long] = runUntilOutput(state) match {
      case Some(newState) => rec(newState)
      case None => state.outputs.reverse
    }

    rec(initialState)
  }

  def runUntilOutput(data: List[Long], inputInstructions: List[Long]): Option[State] = {
    val indexToValue = data.zipWithIndex.map { case (value, idx) => (idx, value) }.toMap
      .withDefaultValue(0L)

    val state = State(indexToValue, inputInstructions, List(), 0, 0)

    runUntilOutput(state)
  }

  @tailrec
  def runUntilOutput(state: State): Option[State] = {

    val latest = state.state
    val pointer = state.pointer
    val relativeBase = state.relativeBase

    val (opCode, mode1, mode2, mode3) = parseOpCodeAndModes(latest(pointer))

    def read(mode: ParameterMode, i: Int): Long = mode match {
      case PositionMode => latest(latest(pointer + i).toInt)
      case ImmediateMode => latest(pointer + i)
      case RelativeMode => latest(latest(pointer + i).toInt + relativeBase)
    }

    def write(mode: ParameterMode, i: Int, value: Long): Map[Int, Long] =
      mode match {
        case PositionMode => latest.updated(latest(pointer + i).toInt, value)
        case ImmediateMode => throw new IllegalArgumentException("Trying to write in immediate mode")
        case RelativeMode => latest.updated(latest(pointer + i).toInt + relativeBase, value)
      }

    opCode match {
      // Add
      case 1 =>
        val newValue = read(mode1, 1) + read(mode2, 2)
        val newLatest = write(mode3, 3, newValue)
        runUntilOutput(state.copy(state = newLatest, pointer = pointer + 4))

      // Multiply
      case 2 =>
        val newValue = read(mode1, 1) * read(mode2, 2)
        val newLatest = write(mode3, 3, newValue)
        runUntilOutput(state.copy(state = newLatest, pointer = pointer + 4))

      // Opcode 3 takes a single integer as input and saves it to the position given by its only parameter.
      // For example, the instruction 3,50 would take an input value and store it at address 50.
      case 3 =>
        val input :: newInputs = state.inputInstructions
        val newLatest = write(mode1, 1, input)
        runUntilOutput(state.copy(state = newLatest, pointer = pointer + 2, inputInstructions = newInputs))

      // Opcode 4 outputs the value of its only parameter.
      // For example, the instruction 4,50 would output the value at address 50.
      case 4 =>
        val output = read(mode1, 1)
        val newOutputs = output :: state.outputs
        Some(state.copy(pointer = pointer + 2, outputs = newOutputs))

      // Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
      case 5 =>
        if (read(mode1, 1) != 0) runUntilOutput(state.copy(pointer = read(mode2, 2).toInt))
        else runUntilOutput(state.copy(pointer = pointer + 3))

      // Opcode 6 is jump-if-false: if the first parameter is zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
      case 6 =>
        if (read(mode1, 1) == 0) runUntilOutput(state.copy(pointer = read(mode2, 2).toInt))
        else runUntilOutput(state.copy(pointer = pointer + 3))

      // Opcode 7 is less than: if the first parameter is less than the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
      case 7 =>
        val newValue = if (read(mode1, 1) < read(mode2, 2)) 1 else 0
        val newLatest = write(mode3, 3, newValue)
        runUntilOutput(state.copy(state = newLatest, pointer = pointer + 4))

      // Opcode 8 is equals: if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
      case 8 =>
        val newValue = if (read(mode1, 1) == read(mode2, 2)) 1 else 0
        val newLatest = write(mode3, 3, newValue)
        runUntilOutput(state.copy(state = newLatest, pointer = pointer + 4))

      // Opcode 9 adjusts the relative base by the value of its only parameter
      case 9 =>
        val relativeBaseAdjustment = read(mode1, 1).toInt
        runUntilOutput(state.copy(pointer = pointer + 2, relativeBase = relativeBase + relativeBaseAdjustment))

      case 99 => None
    }
  }

  def parseOpCodeAndModes(value: Long): (Int, ParameterMode, ParameterMode, ParameterMode) = {
    val opCode = value % 100

    def mapToMode(i: Long): ParameterMode = i match {
      case 0 => PositionMode
      case 1 => ImmediateMode
      case 2 => RelativeMode
    }

    val mode1 = value / 100 % 10
    val mode2 = value / 1000 % 10
    val mode3 = value / 10000 % 10

    (opCode.toInt, mapToMode(mode1), mapToMode(mode2), mapToMode(mode3))
  }
}
