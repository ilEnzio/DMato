package poker

import poker.OrderInstances._

case class ShowDown()

object ShowDown {
  def apply(hands: List[Hand]): List[Hand] =
    hands.sorted.reverse

  /// Order the hands by HankRank
  // Group the hands by HandRank
  // Sort the individual groups
  // flatten the list

}
