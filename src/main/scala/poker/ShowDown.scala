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
      .flatMap(_._2)
    grouped
  }
}
