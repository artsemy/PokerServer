package com.bootcamp
package room.actor

import room.bean.Message.{mAddPlayer, mBigBlind, mMakeBet, mOpenRoom, mRoundStep, mSetHands, mSmallBlind, mStartGame}
import room.bean.Player

import akka.actor.{Actor, ActorRef, Props}

final class ServerActor extends Actor {

  private val roomActor: ActorRef = context.actorOf(Props[RoomActor](), "roomActor")

  override def receive: Receive = {
    case m: mOpenRoom => openRoom()
    case m: mAddPlayer => addPlayer(m.player)
    case m: mStartGame => startGame()
    case m: mMakeBet => makeBet(m.player, m.bet)
  }

  private def openRoom(): Unit = roomActor ! mOpenRoom()
  private def addPlayer(player: Player): Unit = roomActor ! mAddPlayer(player)
  private def startGame(): Unit = {
    roomActor ! mSmallBlind()
    roomActor ! mBigBlind()
    roomActor ! mSetHands()
  }
  private def makeBet(pl: Player, bet: Int): Unit = roomActor ! mRoundStep(bet)

}
