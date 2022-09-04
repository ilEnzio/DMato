package poker

import cats.implicits._

import scala.annotation.tailrec

import OrderInstances._

sealed trait HandRank {
  val score: Int
}

object HandRank {

  val all: List[HandRank] =
    List(StraightFlush, FourOfAKind, FullHouse, Flush, Straight, ThreeOfAKind, TwoPair, Pair, HighCard)

  def apply(hand: Hand): HandRank =
    hand match {
      case x if isStraightFlush(x)     => StraightFlush
      case x if atLeastFourOfAKind(x)  => FourOfAKind
      case x if atLeastFullHouse(x)    => FullHouse
      case x if atLeastFlush(x)        => Flush
      case x if atLeastStraight(x)     => Straight
      case x if atLeastThreeOfAKind(x) => ThreeOfAKind
      case x if atLeastTwoPair(x)      => TwoPair
      case x if atLeastPair(x)         => Pair
      case _                           => HighCard
    }

  private def isStraightFlush(hand: Hand): Boolean = {
    val suitMap = hand.cards
      .groupBy(c => c.suit)
      .filter { case (_, cards) => cards.length >= 5 }
    if (suitMap.isEmpty) false
    else
      atLeastStraight(Hand(suitMap.toList.head._2))
  }

  private def atLeastFourOfAKind(hand: Hand): Boolean =
    hand.cards.groupBy(c => c.rank).exists { case (_, cards) =>
      cards.length == 4
    }

  private def atLeastFullHouse(hand: Hand): Boolean = {
    val count = hand.cards.groupBy(_.rank).toList.map(_._2.size).sorted.reverse
    if (count.size < 2) false
    else if (count.take(2).sum >= 5) true
    else false
  }

  private def atLeastFlush(hand: Hand): Boolean =
    hand.cards
      .groupBy(c => c.suit)
      .exists({ case (_, cards) =>
        cards.length >= 5
      })

  private def atLeastStraight(hand: Hand): Boolean = {
    val sorted        = hand.cards.map(_.rank).distinct.sorted.reverse
    val wheelStraight = List(Ace, Five, Four, Three, Two)
    def check4Str: Boolean = {
      val strPair = sorted.zip(sorted.drop(4))
      strPair.foldLeft(false) { (s, v) =>
        if (s) true
        else v._1.value == v._2.value + 4
      }
    }
    check4Str || (sorted.count(wheelStraight.contains(_)) == 5)
  }

  private def atLeastThreeOfAKind(hand: Hand): Boolean =
    hand.cards.groupBy(c => c.rank).exists { case (_, cards) =>
      cards.length == 3
    }

  private def atLeastTwoPair(hand: Hand): Boolean =
    hand.cards
      .groupBy(c => c.rank)
      .count { case (_, cards) =>
        cards.size == 2
      } >= 2

  private def atLeastPair(hand: Hand): Boolean =
    hand.cards
      .groupBy(c => c.rank)
      .count { case (_, cards) =>
        cards.size == 2
      } >= 1
}

object StraightFlush extends HandRank { val score = 9 }
object FourOfAKind   extends HandRank { val score = 8 }
object FullHouse     extends HandRank { val score = 7 }
object Flush         extends HandRank { val score = 6 }
object Straight      extends HandRank { val score = 5 }
object ThreeOfAKind  extends HandRank { val score = 4 }
object TwoPair       extends HandRank { val score = 3 }
object Pair          extends HandRank { val score = 2 }
object HighCard      extends HandRank { val score = 1 }
