package com.dylanm.adventofcode.day04

import com.dylanm.adventofcode.day04.SecureContainer._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SecureContainerTest extends AnyFlatSpec with Matchers {

  behavior of "SecureContainer"

  it should "check whether digits in int are always increasing and provide next possible candidate" in {
    digitsAreIncreasingAndNextCandidate(111111) shouldBe(true, 111112)
    digitsAreIncreasingAndNextCandidate(12345) shouldBe(true, 12346)
    digitsAreIncreasingAndNextCandidate(223450) shouldBe(false, 223455)
    digitsAreIncreasingAndNextCandidate(230123407) shouldBe(false, 233333333)
  }

  it should "check whether two adjacent digits in int are the same" in {
    twoAdjacentDigitsAreSame(111111) shouldBe true
    twoAdjacentDigitsAreSame(12345) shouldBe false
    twoAdjacentDigitsAreSame(223450) shouldBe true
    twoAdjacentDigitsAreSame(122250) shouldBe true
  }

  it should "check whether ONLY two adjacent digits in int are the same" in {
    onlyTwoAdjacentDigitsAreSame(111111) shouldBe false
    onlyTwoAdjacentDigitsAreSame(12345) shouldBe false
    onlyTwoAdjacentDigitsAreSame(223450) shouldBe true
    onlyTwoAdjacentDigitsAreSame(122250) shouldBe false
    onlyTwoAdjacentDigitsAreSame(111122) shouldBe true
  }

  it should "produce the correct answer for puzzle 1" in {
    puzzle1(138307, 654504) shouldBe 1855
  }

  it should "produce the correct answer for puzzle 2" in {
    puzzle2(138307, 654504) shouldBe 1253
  }

}