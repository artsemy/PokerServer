package com.bootcamp
package room.bean

sealed trait Message {}

object Message{
  final case class mOpenRoom() extends Message
  final case class mAddPlayer(player: Player) extends Message
  final case class mStartGame() extends Message
  final case class mSmallBlind() extends Message
  final case class mBigBlind() extends Message
  final case class mSetHands() extends Message
  final case class mRoundStep(bet: Int) extends Message
  final case class mMakeFlop(bet: Int) extends Message
  final case class mMakeBet(player: Player, bet: Int) extends Message
  final case class mCloseRound() extends Message
}
