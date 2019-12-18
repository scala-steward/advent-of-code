package com.dylanm.adventofcode.day14

import scala.annotation.tailrec
import scala.io.Source

object SpaceStoichiometry {
  def read(filePath: String): Map[String, (Int, List[(String, Int)])] = {
    Source.fromFile(filePath).getLines.toList.map {
      a =>
        a.split(" => ") match {
          case Array(a1, a2) => {
            val resultSplits = a2.trim.split(" ")
            val resultChemical = resultSplits(1)
            val numResults = resultSplits(0).toInt

            val reactantsSplit = a1.split(", ").toList
            val reactantsList = reactantsSplit.map {
              b => (b.split(" ")(1), b.split(" ")(0).toInt)
            }

            resultChemical -> (numResults, reactantsList)
          }
        }
    }.toMap
  }

  def maxAmountOfFuel(reactions: Map[String, (Int, List[(String, Int)])],
                      amountOfOre: Long): Long = {

    def findBinarySearchStartAndEnd: (Long, Long) = {
      @tailrec
      def rec(prev: Long, current: Long): (Long, Long) = {
        val oreNeeded = amountOfOreNeededForFuel(reactions, current)
        if (oreNeeded > amountOfOre) (prev, current)
        else rec(current, Math.exp(current).toInt)
      }

      rec(0, 1)
    }

    val (binarySearchStart, binarySearchEnd) = findBinarySearchStartAndEnd

    @tailrec
    def binarySearch(start: Long, end: Long): Long = {
      val mid = (start + end) / 2

      if (mid == start) start
      else {
        val m = amountOfOreNeededForFuel(reactions, mid)

        if (m > amountOfOre) binarySearch(start, mid)
        else binarySearch(mid, end)
      }
    }

    binarySearch(binarySearchStart, binarySearchEnd)
  }

  def amountOfOreNeededForFuel(reactions: Map[String, (Int, List[(String, Int)])],
                               initialAmount: Long = 1): Long = {

    def rec(chem: String,
            amount: Long = initialAmount,
            excess: Map[String, Long] = Map().withDefaultValue(0)): (Long, Map[String, Long]) =
      if (chem == "ORE") (amount, excess)
      else {
        val amountWithoutExcess = math.max(0L, amount - excess(chem))
        val amountFromExcess = amount - amountWithoutExcess
        val excessWithoutAmount = excess + (chem -> (excess(chem) - amountFromExcess))

        val (outputAmount, chems) = reactions(chem)

        val (reactionRepeat, outputExcess) = (amountWithoutExcess / outputAmount, amountWithoutExcess % outputAmount) match {
          case (q, 0) => (q, 0L)
          case (q, rem) => (q + 1, outputAmount - rem)
        }

        val (ore, inputExcess) = chems.foldLeft((0L, excessWithoutAmount))({
          case ((oreAmount, excess), (inputChemical, inputAmount)) =>
            val (inputOre, inputExcess) = rec(inputChemical, reactionRepeat * inputAmount, excess)
            (oreAmount + inputOre, inputExcess)
        })

        (ore, inputExcess + (chem -> (inputExcess(chem) + outputExcess)))
      }

    rec("FUEL")._1
  }

}
