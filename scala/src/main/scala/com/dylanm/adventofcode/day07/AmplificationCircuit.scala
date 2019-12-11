package com.dylanm.adventofcode.day07

import com.dylanm.adventofcode.utils.{IntCode, State}

import scala.annotation.tailrec
import scala.io.Source

object AmplificationCircuit {
  def maxThrusterSignal(program: List[Long]): Long = {
    val phaseSequences = (0L to 4L).permutations.map(_.toList)
    phaseSequences.map(thrusterSignal(program, _)).max
  }

  def thrusterSignal(program: List[Long], phaseSequence: List[Long]): Long =
    phaseSequence.foldLeft(0L)((res, phase) => IntCode.runIntCode(program, List(phase, res)).head)

  def maxFeedbackLoopThrusterSignal(program: List[Long]): Long = {
    val phaseSequences = (5L to 9L).permutations.map(_.toList)
    phaseSequences.map(feedbackLoopThrusterSignal(program, _)).max
  }

  def feedbackLoopThrusterSignal(program: List[Long], phaseSequence: List[Long]): Long = {

    @tailrec
    def rec(states: List[State], latestOutputs: List[Long]): Long = {
      IntCode.runUntilOutput(states.head.copy(inputInstructions = latestOutputs)) match {
        case Some(newState) => rec(states.tail :+ newState, newState.outputs)
        case None => states.last.outputs.head
      }
    }

    val initialStates = phaseSequence.foldLeft(List[State]())((states, phase) =>
      states match {
        case Nil => IntCode.runUntilOutput(program, List(phase, 0L)).get :: states
        case head :: _ => IntCode.runUntilOutput(program, phase :: head.outputs).get :: states
      }
    ).reverse

    rec(initialStates, initialStates.last.outputs)
  }

  def read(filePath: String): List[Long] =
    Source.fromFile(filePath).getLines.toList.head.split(",")
      .map(_.toLong)
      .toList

}