package deck

sealed trait Ranking {}
object Ranking {
  def apply(hand: Hand): Ranking =
    hand match {
      case x if isFourOfAKind(x)  => FourOfAKind
      case x if isThreeOfAKind(x) => ThreeOfAKind
      case _                      => HighCard
    }

  def isFourOfAKind(hand: Hand): Boolean =
    hand.cards.groupBy(c => c.rank).exists { case (_, cards) =>
      cards.length == 4
    }

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
