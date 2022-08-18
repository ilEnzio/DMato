package poker

import poker.OrderInstances._

case class ShowDown()

object ShowDown {
  def apply(hands: List[Hand]): List[Hand] = {
    //    hands.sorted.reverse

    /// Order the hands by HankRank
    // Group the hands by HandRank
    // Sort the individual groups
    // flatten the list

    val grouped = hands
      .groupBy(HandRank(_))
      .toList
      .sortBy(_._1)
      .reverse

    grouped.flatMap(g =>
      g._1 match {
        case Pair => evaluatePairs(g._2)
        case _    => evaluateHighCard(g._2)
      }
    )

  }

  private def evaluateHighCard(value: List[Hand]): List[Hand] =
    value.sorted.reverse

  private def evaluatePairs(value: List[Hand]): List[Hand] =
    value.sorted(pairOrdering).reverse
}
