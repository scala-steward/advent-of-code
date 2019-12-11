package com.dylanm.adventofcode.day04

import scala.annotation.tailrec

object SecureContainer {

  def puzzle1(start: Int, end: Int): Int = genericPuzzle(start, end, twoAdjacentDigitsAreSame)

  def twoAdjacentDigitsAreSame(i: Int): Boolean = {
    val listified = intToDigitList(i)

    @tailrec
    def rec(previousDigit: Int, remainingDigits: List[Int]): Boolean = {
      remainingDigits match {
        case Nil => false
        case x :: _ if x == previousDigit => true
        case x :: xs => rec(x, xs)
      }
    }

    rec(listified.head, listified.tail)
  }

  private def intToDigitList(i: Int): List[Int] = i.toString.toList.map(_ - '0')

  private def genericPuzzle(start: Int, end: Int, f: (Int) => Boolean): Int = {
    @tailrec
    def rec(curr: Int, numSoFar: Int = 0): Int = {
      if (curr > end) numSoFar
      else {
        val (increasing, nextCandidate) = digitsAreIncreasingAndNextCandidate(curr)

        if (increasing && f(curr)) rec(nextCandidate, numSoFar + 1)
        else if (increasing) rec(nextCandidate, numSoFar)
        else rec(nextCandidate, numSoFar)
      }
    }

    rec(start)
  }

  /**
   * Return whether all digits are increasing and the next candidate where this might be the case.
   * If an decrease in digit is found, then int is filled with the last increasing digit to
   * produce the next candidate
   */
  def digitsAreIncreasingAndNextCandidate(i: Int): (Boolean, Int) = {
    val listified = intToDigitList(i)

    @tailrec
    def rec(previousDigit: Int,
            remainingDigits: List[Int],
            digitsSoFar: List[Int] = List()): (Boolean, Int) = {
      remainingDigits match {
        case Nil => (true, i + 1)
        case x :: _ if x < previousDigit => {
          val nextCandidate =
            ((previousDigit :: digitsSoFar).reverse ++ List.fill(remainingDigits.size)(previousDigit))
              .mkString("")
              .toInt
          (false, nextCandidate)
        }
        case x :: xs => rec(x, xs, previousDigit :: digitsSoFar)
      }
    }

    rec(listified.head, listified.tail)
  }

  def puzzle2(start: Int, end: Int): Int = genericPuzzle(start, end, onlyTwoAdjacentDigitsAreSame)

  def onlyTwoAdjacentDigitsAreSame(i: Int): Boolean = {
    val listified = intToDigitList(i)

    @tailrec
    def rec(previousDigit: Int, remainingDigits: List[Int], numConsecutive: Int = 1): Boolean = {
      remainingDigits match {
        case Nil => numConsecutive == 2
        case x :: xs =>
          if (x == previousDigit) rec(x, xs, numConsecutive + 1)
          else if (numConsecutive == 2) true
          else rec(x, xs)
      }
    }

    rec(listified.head, listified.tail)
  }

}