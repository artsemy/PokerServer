package com.bootcamp
package room.bean

sealed trait Combination {}

object Combination{
  final case class HighCard(fiveCard: FiveCard) extends Combination
  final case class Pair(fiveCard: FiveCard) extends Combination
  final case class TwoPairs(fiveCard: FiveCard) extends Combination
  final case class ThreeOfaKind(fiveCard: FiveCard) extends Combination
  final case class Straight(fiveCard: FiveCard) extends Combination
  final case class Flush(fiveCard: FiveCard) extends Combination
  final case class FullHouse(fiveCard: FiveCard) extends Combination
  final case class FourOfaKind(fiveCard: FiveCard) extends Combination
  final case class StraightFlush(fiveCard: FiveCard) extends Combination
  final case class RoyalFlush(fiveCard: FiveCard) extends Combination
}