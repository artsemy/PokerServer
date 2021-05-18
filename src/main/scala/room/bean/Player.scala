package com.bootcamp
package room.bean

case class Player(id: Long) {

  private var money: Int = 0
  private var hand: Hand = null

  def getMoney(): Int = {
    money
  }

  def updateMoney(diff: Int): Unit = {
    money += diff
  }

  def setHand(hand: Hand): Unit = {
    this.hand = hand
  }

}
