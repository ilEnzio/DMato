package deck

sealed trait Ranking {}
// I'm thinking now that maybe it should have a value
// of the actual n<=5 cards that make up the Ranking.  That way it can be used later
// like with UI
object Ranking {

  // TODo I kinda think this is wrong now.  I need a function that goes from
  // Hand => Int, then I can just order the hands and I might not need this type??
  def apply(hand: Hand): Ranking =
    hand match {
      case x if isFourOfAKind(x)  => FourOfAKind
      case x if isFlush(x)        => Flush
      case x if isThreeOfAKind(x) => ThreeOfAKind
      case _                      => HighCard
    }

  def isFourOfAKind(hand: Hand): Boolean =
    hand.cards.groupBy(c => c.rank).exists { case (_, cards) =>
      cards.length == 4
    }

  def isFlush(hand: Hand): Boolean =
    // and not a STraight!
    hand.cards
      .groupBy(c => c.suit)
      .exists({ case (_, cards) =>
        cards.length >= 5
      })

  def isThreeOfAKind(hand: Hand): Boolean =
    !isFourOfAKind(hand) &&
      hand.cards.groupBy(c => c.rank).exists { case (_, cards) =>
        cards.length == 3
      }
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
