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
      case x if isStraight(x)     => Straight
      case x if isThreeOfAKind(x) => ThreeOfAKind
      case _                      => HighCard
    }

  def isFourOfAKind(hand: Hand): Boolean =
    hand.cards.groupBy(c => c.rank).exists { case (_, cards) =>
      cards.length == 4
    }

  def isFlush(hand: Hand): Boolean =
    // and not also Straight!
    hand.cards
      .groupBy(c => c.suit)
      .exists({ case (_, cards) =>
        cards.length >= 5
      })

  // TODO not tested!
  def isStraight(hand: Hand): Boolean = {
    // and not also a Flush!
    // distinctBy, handle Ace, sort
    // take 5
    val culled = hand.cards.distinctBy(c => c.rank.value)

//    println(s"Culled: $culled")
    //    println(sorted)
    def isTooShort(cards: List[Card]) = cards.length < 5
    if (isTooShort(culled)) false
    else {
      def handleAce: List[Card] =
        if (hand.cards.exists { case c => c.rank == Ace })
          hand.cards.find(_.rank == Ace).get.copy(rank = Ace_L) :: culled
        else
          culled

      val checkedForAce = handleAce
//      println(s"Checked4A: $checkedForAce")
      val sorted = checkedForAce.sortBy(_.rank.value).reverse
//      println(s"Sorted: $sorted")
      def checkStr(cards: List[Card]): Boolean =
        if (isTooShort(cards)) false
        else {
          if (cards(0).rank.value == cards(4).rank.value + 4) !isFlush(hand) && true
          else checkStr(cards.drop(1))
        }

      checkStr(sorted)
    }
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
