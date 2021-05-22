package com.bootcamp
package room.bean

case class Player(id: Long) {

  private var money: Int = 0
  private var hand: Hand = null
  private var currentBet = 0

  def getMoney(): Int = {
    money
  }

  def updateMoney(diff: Int): Unit = {
    money += diff
  }

  def setHand(hand: Hand): Unit = {
    this.hand = hand
  }

  def getHand(): Hand = hand

  def setCurrentBet(bet: Int) = { currentBet = bet }

  def getCurrentBet() = currentBet
}
