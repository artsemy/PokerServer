package com.bootcamp
package room.bean

case class FiveCard(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) {
  def w1: Int = card1.rank.getWeight
  def w2: Int = card2.rank.getWeight
  def w3: Int = card3.rank.getWeight
  def w4: Int = card4.rank.getWeight
  def w5: Int = card5.rank.getWeight

  def s1: Suit = card1.suit
  def s2: Suit = card2.suit
  def s3: Suit = card3.suit
  def s4: Suit = card4.suit
  def s5: Suit = card5.suit
}
