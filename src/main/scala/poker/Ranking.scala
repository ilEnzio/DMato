package poker

import scala.annotation.tailrec

sealed trait Ranking {}

object Ranking {
  def all: List[Ranking] =
    List(StraightFlush, FourOfAKind, FullHouse, Flush, Straight, ThreeOfAKind, TwoPair, Pair, HighCard)

  def apply(hand: Hand): Ranking =
    // TODO seems a little fragile because it is depending on the testing order,
    // does being private make this ok?
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
    else atLeastStraight(Hand(suitMap.toList.flatMap(_._2)))
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
    val culled = hand.cards.distinctBy(c => c.rank.value)

    def isTooShort(cards: List[Card]) = cards.length < 5
    if (isTooShort(culled)) false
    else {
      def handleAce: List[Card] =
        if (hand.cards.exists { case c => c.rank == Ace })
          hand.cards.find(_.rank == Ace).get.copy(rank = PhantomAce) :: culled
        else
          culled

      val checkedForAce = handleAce
      val sorted        = checkedForAce.sortBy(_.rank.value).reverse

      @tailrec
      def checkStr(cards: List[Card]): Boolean =
        if (isTooShort(cards)) false
        else {
          if (cards(0).rank.value == cards(4).rank.value + 4) true
          else checkStr(cards.drop(1))
        }

      checkStr(sorted)
    }
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

object StraightFlush extends Ranking
object FourOfAKind   extends Ranking
object FullHouse     extends Ranking
object Flush         extends Ranking
object Straight      extends Ranking
object ThreeOfAKind  extends Ranking
object TwoPair       extends Ranking
object Pair          extends Ranking
object HighCard      extends Ranking
