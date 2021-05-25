package com.bootcamp
package room.util

import room.bean.{Board, Card, Combination, FiveCard, Hand}

import scala.collection.mutable

case class CountBestCombination() {

  val sevenCards: mutable.Queue[Card] = mutable.Queue()

  def findBestCombination(board: Board, hand: Hand): Combination = {
    initSevenCards(board, hand)
    findBestPossible()
  }

  private def findBestPossible(): Combination = {
    var comb: Combination = null
    var indexes = 34567
    var n = 5
    var m = 4
    var maxWeight = 0
    while (n > 0) {
      var sum = 0
      while (m > -1) {
        val fiveCard: FiveCard = BuildCombinationFromIndexes(indexes)
        val combination: Combination = DefineCombinationType(fiveCard)
        val weight: Int = combination.weight
        if (weight > maxWeight) {
          comb = combination
          maxWeight = weight
        }
        indexes = indexes - Math.pow(10, m).toInt
        m -= 1
        sum += Math.pow(10, m).toInt
      }
      val fiveCard: FiveCard = BuildCombinationFromIndexes(indexes)
      val combination: Combination = DefineCombinationType(fiveCard)
      val weight: Int = combination.weight
      if (weight > maxWeight) {
        comb = combination
        maxWeight = weight
      }
      n -= 1
      indexes = indexes - (Math.pow(10, n).toInt - sum)
      m = n - 1
    }
    indexes = 12345
    val fiveCard: FiveCard = BuildCombinationFromIndexes(indexes)
    val combination: Combination = DefineCombinationType(fiveCard)
    val weight: Int = combination.weight
    if (weight > maxWeight) {
      comb = combination
      maxWeight = weight
    }
    comb
  }

  private def DefineCombinationType(fiveCard: FiveCard): Combination = {
    if (Combination.isRoyalFlush(fiveCard)) Combination.RoyalFlush(fiveCard)
    else if (Combination.isStraightFlush(fiveCard)) Combination.StraightFlush(fiveCard)
    else if (Combination.isFourOfaKind(fiveCard)) Combination.FourOfaKind(fiveCard)
    else if (Combination.isFullHouse(fiveCard)) Combination.FullHouse(fiveCard)
    else if (Combination.isFlush(fiveCard)) Combination.Flush(fiveCard)
    else if (Combination.isStraight(fiveCard)) Combination.Straight(fiveCard)
    else if (Combination.isThreeOfaKind(fiveCard)) Combination.ThreeOfaKind(fiveCard)
    else if (Combination.isTwoPairs(fiveCard)) Combination.TwoPairs(fiveCard)
    else if (Combination.isPair(fiveCard)) Combination.Pair(fiveCard)
    else Combination.HighCard(fiveCard)
  }

  private def BuildCombinationFromIndexes(i: Int): FiveCard = {
    var p = i
    val index5 = p%10 - 1
    p /= 10
    val index4 = p%10 - 1
    p /= 10
    val index3 = p%10 - 1
    p /= 10
    val index2 = p%10 - 1
    p /= 10
    val index1 = p - 1
    FiveCard(sevenCards(index1), sevenCards(index2), sevenCards(index3), sevenCards(index4), sevenCards(index5))
  }

  private def initSevenCards(board: Board, hand: Hand): Unit = {
    sevenCards.append(board.fiveCard.card1)
    sevenCards.append(board.fiveCard.card2)
    sevenCards.append(board.fiveCard.card3)
    sevenCards.append(board.fiveCard.card4)
    sevenCards.append(board.fiveCard.card5)
    sevenCards.append(hand.card1)
    sevenCards.append(hand.card2)
    sevenCards.sortInPlaceBy(c => c.rank.getWeight)
  }
}
