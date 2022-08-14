package deck

import scala.annotation.tailrec

sealed trait Ranking {}
// I'm thinking now that maybe it should have a value
// of the actual n<=5 cards that make up the Ranking.  That way it can be used later
// like with UI
object Ranking {

  // TODo I kinda think this is wrong now.  I need a function that goes from
  // Hand => Int, then I can just order the hands and I might not need this type??
  def apply(hand: Hand): Ranking =
    // TODO seems a little fragile because it is depending on the testing order,
    // does being private make this ok?
    hand match {
      case x if isStraightFlush(x) => StraightFlush
      case x if isFourOfAKind(x)   => FourOfAKind
      case x if isFullHouse(x)     => FullHouse
      case x if isFlush(x)         => Flush
      case x if isStraight(x)      => Straight
      case x if isThreeOfAKind(x)  => ThreeOfAKind
      case x if isTwoPair(x)       => TwoPair
      case _                       => HighCard
    }

  private def isStraightFlush(hand: Hand): Boolean = {
    val suitMap = hand.cards
      .groupBy(c => c.suit)
      .filter { case (_, cards) => cards.length >= 5 }
    if (suitMap.isEmpty) false
    else isStraight(Hand(suitMap.toList.flatMap(_._2)))
  }

  private def isFourOfAKind(hand: Hand): Boolean =
    hand.cards.groupBy(c => c.rank).exists { case (_, cards) =>
      cards.length == 4
    }

  private def isFullHouse(hand: Hand): Boolean = {
    val count = hand.cards.groupBy(_.rank).toList.map(_._2.size).sorted.reverse
    if (count.size < 2) false
    else if (count.take(2).sum >= 5) true
    else false

  }

  private def isFlush(hand: Hand): Boolean =
    hand.cards
      .groupBy(c => c.suit)
      .exists({ case (_, cards) =>
        cards.length >= 5
      })

  private def isStraight(hand: Hand): Boolean = {
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

  private def isThreeOfAKind(hand: Hand): Boolean =
    hand.cards.groupBy(c => c.rank).exists { case (_, cards) =>
      cards.length == 3
    }

  private def isTwoPair(hand: Hand): Boolean =
    hand.cards
      .groupBy(c => c.rank)
      .count { case (_, cards) =>
        cards.size == 2
      } >= 2
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
