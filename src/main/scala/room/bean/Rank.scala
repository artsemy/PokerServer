package com.bootcamp
package room.bean

sealed abstract class Rank(weight: Int) {}

object Rank{
  final case class Two() extends Rank(2)
  final case class Three() extends Rank(3)
  final case class Four() extends Rank(4)
  final case class Five() extends Rank(5)
  final case class Six() extends Rank(6)
  final case class Seven() extends Rank(7)
  final case class Eight() extends Rank(8)
  final case class Nine() extends Rank(9)
  final case class Ten() extends Rank(10)
  final case class Jack() extends Rank(11)
  final case class Queen() extends Rank(12)
  final case class King() extends Rank(13)
  final case class Ace() extends Rank(14)
}
