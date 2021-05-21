package com.bootcamp
package room.bean

sealed trait Combination {
  def weight: Int
  def isCombination: Boolean
}

object Combination {

  final case class HighCard(fiveCard: FiveCard) extends Combination {
    override def weight: Int = {
      var res = fiveCard.w1
      res += fiveCard.w2 * 8
      res += fiveCard.w3 * 64
      res += fiveCard.w4 * 512
      res += fiveCard.w5 * 4096
      res
    }
    override def isCombination: Boolean = true
  }

  final case class Pair(fiveCard: FiveCard) extends Combination {
    private val c = getAllWeight(fiveCard)
    override def weight: Int = {
      var index1, index2, index3, index4 = 0
      if (fiveCard.w2 - fiveCard.w1 == 0) {
        index1 = 2
        index2 = 3
        index3 = 4
        index4 = 0
      }
      if (fiveCard.w3 - fiveCard.w2 == 0) {
        index1 = 0
        index2 = 3
        index3 = 4
        index4 = 1
      }
      if (fiveCard.w4 - fiveCard.w3 == 0) {
        index1 = 0
        index2 = 1
        index3 = 4
        index4 = 2
      }
      if (fiveCard.w5 - fiveCard.w4 == 0) {
        index1 = 0
        index2 = 1
        index3 = 2
        index4 = 3
      }
      c(index1) + c(index2)*9 + c(index3)*81 + c(index4)*911 + 64866
    }
    override def isCombination: Boolean = {
      var p = 0
      if (fiveCard.w2 - fiveCard.w1 == 0) p += 1
      if (fiveCard.w3 - fiveCard.w2 == 0) p += 1
      if (fiveCard.w4 - fiveCard.w3 == 0) p += 1
      if (fiveCard.w5 - fiveCard.w4 == 0) p += 1
      p == 1
    }
  }

  final case class TwoPairs(fiveCard: FiveCard) extends Combination {
    override def weight: Int = {
      val c = getAllWeight(fiveCard)
      var index1, index2, index3 = 0
      if (fiveCard.w2 - fiveCard.w1 == 0 &&
        fiveCard.w4 - fiveCard.w3 == 0) {
        index1 = 4
        index2 = 0
        index3 = 2
      }
      if (fiveCard.w2 - fiveCard.w1 == 0 &&
        fiveCard.w5 - fiveCard.w4 == 0) {
        index1 = 2
        index2 = 0
        index3 = 3
      }
      if (fiveCard.w3 - fiveCard.w2 == 0 &&
        fiveCard.w5 - fiveCard.w4 == 0) {
        index1 = 0
        index2 = 1
        index3 = 3
      }
      c(index1) + c(index2)*13 + c(index3)*139 + 78792
    }
    override def isCombination: Boolean = {
      var p = 0
      if (fiveCard.w2 - fiveCard.w1 == 0) p += 1
      if (fiveCard.w3 - fiveCard.w2 == 0) p += 1
      if (fiveCard.w4 - fiveCard.w3 == 0) p += 1
      if (fiveCard.w5 - fiveCard.w4 == 0) p += 1
      p == 2
    }
  }

  final case class ThreeOfaKind(fiveCard: FiveCard) extends Combination {
    override def weight: Int = {
      val c = getAllWeight(fiveCard)
      var index1, index2, index3 = 0
      if (fiveCard.w2 - fiveCard.w1 == 0 &&
        fiveCard.w3 - fiveCard.w2 == 0) {
        index1 = 3
        index2 = 4
        index3 = 0
      }
      if (fiveCard.w3 - fiveCard.w2 == 0 &&
        fiveCard.w4 - fiveCard.w3 == 0) {
        index1 = 0
        index2 = 4
        index3 = 1
      }
      if (fiveCard.w4 - fiveCard.w3 == 0 &&
        fiveCard.w5 - fiveCard.w4 == 0) {
        index1 = 0
        index2 = 1
        index3 = 2
      }
      c(index1) + c(index2)*11 + c(index3)*133 + 80919
    }
    override def isCombination: Boolean = {
      var p = 0
      if (fiveCard.w2 - fiveCard.w1 == 0 &&
        fiveCard.w3 - fiveCard.w2 == 0) p = 1
      if (fiveCard.w3 - fiveCard.w2 == 0 &&
        fiveCard.w4 - fiveCard.w3 == 0) p = 1
      if (fiveCard.w4 - fiveCard.w3 == 0 &&
        fiveCard.w5 - fiveCard.w4 == 0) p = 1
      p == 1
    }
  }

  final case class Straight(fiveCard: FiveCard) extends Combination {
    override def weight: Int = {
      var p = 0
      if (fiveCard.w5 == 14 && fiveCard.w1 == 2) p = 5
      else p = fiveCard.w5
      p + 82936
    }
    override def isCombination: Boolean = {
      var p = 0
      if (fiveCard.w2 - fiveCard.w1 == 1) p += 1
      if (fiveCard.w3 - fiveCard.w2 == 1) p += 1
      if (fiveCard.w4 - fiveCard.w3 == 1) p += 1
      if (fiveCard.w5 - fiveCard.w4 == 1 ||
        fiveCard.w5 - fiveCard.w1 == 12) p += 1
      p == 4
    }
  }

  final case class Flush(fiveCard: FiveCard) extends Combination {
    override def weight: Int = {
      var res = fiveCard.w1
      res += fiveCard.w2 * 8
      res += fiveCard.w3 * 64
      res += fiveCard.w4 * 512
      res += fiveCard.w5 * 4096
      res + 82950
    }
    override def isCombination: Boolean = {
      var p = 0
      if (fiveCard.s1 == fiveCard.s2) p += 1
      if (fiveCard.s2 == fiveCard.s3) p += 1
      if (fiveCard.s3 == fiveCard.s4) p += 1
      if (fiveCard.s4 == fiveCard.s5) p += 1
      p == 4
    }
  }

  final case class FullHouse(fiveCard: FiveCard) extends Combination {
    override def weight: Int = {
      val c = getAllWeight(fiveCard)
      var index1 = 0
      if(fiveCard.w2 == fiveCard.w3) index1 = 3
      else index1 = 1
      c(index1) + c(2)*13 + 147816
    }
    override def isCombination: Boolean = {
      var p = 0
      if (fiveCard.w1 == fiveCard.w2 &&
        fiveCard.w3 == fiveCard.w4 && 
        fiveCard.w4 == fiveCard.w5) p += 1
      if (fiveCard.w1 == fiveCard.w2 &&
        fiveCard.w2 == fiveCard.w3 &&
        fiveCard.w4 == fiveCard.w5) p += 1
      p == 1
    }
  }

  final case class FourOfaKind(fiveCard: FiveCard) extends Combination {
    override def weight: Int = {
      var index1 = 0
      val c = getAllWeight(fiveCard)
      if (fiveCard.w1 == fiveCard.w2 && fiveCard.w2 == fiveCard.w3 &&
        fiveCard.w3 == fiveCard.w4) index1 = 4
      else index1 = 0
      c(index1) + c(2)*13 + 148011
    }
    override def isCombination: Boolean = {
      var p = 0
      if (fiveCard.w1 == fiveCard.w2 && fiveCard.w2 == fiveCard.w3 &&
        fiveCard.w3 == fiveCard.w4) p += 1
      if (fiveCard.w2 == fiveCard.w3 && fiveCard.w3 == fiveCard.w4 &&
        fiveCard.w4 == fiveCard.w5) p += 1
      p == 1
    }
  }

  final case class StraightFlush(fiveCard: FiveCard) extends Combination {
    override def weight: Int = {
      var p = 0
      if (fiveCard.w5 == 14 && fiveCard.w1 == 2) p = 5
      else p = fiveCard.w5
      p + 148206
    }
    override def isCombination: Boolean = {
      var p = 0
      if (Straight(fiveCard).isCombination) p += 1
      if (Flush(fiveCard).isCombination) p += 1
      p == 2
    }
  }

  final case class RoyalFlush(fiveCard: FiveCard) extends Combination {
    override def weight: Int = {
      1 + 148220
    }
    override def isCombination: Boolean = {
      StraightFlush(fiveCard).isCombination && fiveCard.w4 == 13
    }
  }

  def getAllWeight(fiveCard: FiveCard): List[Int] = {
    List(fiveCard.w1, fiveCard.w2,
      fiveCard.w3, fiveCard.w4, fiveCard.w5)
  }
  def getAllSuit(fiveCard: FiveCard): List[Suit] = {
    List(fiveCard.s1, fiveCard.s2,
      fiveCard.s3, fiveCard.s4, fiveCard.s5)
  }
}