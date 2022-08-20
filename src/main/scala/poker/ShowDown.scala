package poker
import OrderInstances._

case class ShowDown()

object ShowDown {
  def apply(hands: List[Hand]): List[Hand] = {
    /// Order the hands by HankRank
    // Group the hands by HandRank
    // Sort the individual groups
    // flatten the list

    val grouped = hands
      .groupBy(HandRank(_))
      .toList
      .sortBy(_._1)
      .reverse

    grouped.flatMap { g =>
      g._1 match {
        case Straight     => evaluateStraight(g._2)
        case ThreeOfAKind => evaluateThreeOfKind(g._2)
        case TwoPair      => evaluateTwoPair(g._2)
        case Pair         => evaluatePairs(g._2)
        case _            => evaluateHighCard(g._2)
      }
    }

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

}
