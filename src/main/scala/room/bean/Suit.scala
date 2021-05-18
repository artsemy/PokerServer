package com.bootcamp
package room.bean

sealed trait Suit {}

object Suit{
  final case object Hearts extends Suit
  final case object Diamonds extends Suit
  final case object Spades extends Suit
  final case object Clubs extends Suit
}
