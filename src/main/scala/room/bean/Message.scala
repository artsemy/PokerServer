package com.bootcamp
package room.bean

sealed trait Message {}

object Message{
  final case class mOpenRoom() extends Message
  final case class mAddPlayer() extends Message
  final case class mSmallBlind() extends Message
  final case class mBigBlind() extends Message
  final case class mSetHands() extends Message
  final case class mPreFlop() extends Message
  final case class mMakeFlop() extends Message
}
