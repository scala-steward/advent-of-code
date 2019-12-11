package com.dylanm.adventofcode.day11

sealed trait Direction {
  def clockwise: Direction

  def counterClockwise: Direction
}

case object Up extends Direction {
  override def clockwise: Direction = Right

  override def counterClockwise: Direction = Left
}

case object Down extends Direction {
  override def clockwise: Direction = Left

  override def counterClockwise: Direction = Right
}

case object Right extends Direction {
  override def clockwise: Direction = Down

  override def counterClockwise: Direction = Up
}

case object Left extends Direction {
  override def clockwise: Direction = Up

  override def counterClockwise: Direction = Down
}
