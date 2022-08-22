package poker
import OrderInstances._

case class ShowDown()

object ShowDown {
  def apply(hands: List[Hand]): List[Hand] = {

    val grouped = hands
      .groupBy(HandRank(_))
      .toList
      .sortBy(_._1)
      .reverse

    for {
      (rankCategory, hands) <- grouped
      evaluation <- rankCategory match {
        case StraightFlush => evaluateStraightFlush(hands)
        case FourOfAKind   => evaluateFourOfAKind(hands)
        case FullHouse     => evaluateFullHouse(hands)
        case Flush         => evaluateFlush(hands)
        case Straight      => evaluateStraight(hands)
        case ThreeOfAKind  => evaluateThreeOfKind(hands)
        case TwoPair       => evaluateTwoPair(hands)
        case Pair          => evaluatePairs(hands)
        case _             => evaluateHighCard(hands)
      }
    } yield evaluation

  }

  private def evaluateHighCard(value: List[Hand]): List[Hand] =
    value.sorted.reverse

  private def evaluatePairs(value: List[Hand]): List[Hand] =
    value.sorted(pairOrdering).reverse

  private def evaluateTwoPair(value: List[Hand]): List[Hand] =
    value.sorted(twoPairOrdering).reverse

  private def evaluateThreeOfKind(value: List[Hand]): List[Hand] =
    value.sorted(threeOfAKindOrdering).reverse

  private def evaluateStraight(value: List[Hand]): List[Hand] =
    value.sorted(straightOrdering).reverse

  private def evaluateFlush(value: List[Hand]): List[Hand] =
    value.sorted(flushOrdering).reverse

  private def evaluateFullHouse(value: List[Hand]): List[Hand] =
    value.sorted(fullHouseOrdering).reverse

  private def evaluateFourOfAKind(value: List[Hand]): List[Hand] =
    value.sorted(fourOfAKindOrdering).reverse

  private def evaluateStraightFlush(value: List[Hand]): List[Hand] =
    value.sorted(straightFlushOrdering).reverse
}
