package com.dylanm.adventofcode.utils

sealed trait ParameterMode

case object PositionMode extends ParameterMode

case object ImmediateMode extends ParameterMode

case object RelativeMode extends ParameterMode
