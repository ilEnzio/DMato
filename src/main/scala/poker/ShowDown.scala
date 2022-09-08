package poker

import cats.implicits.toFoldableOps
import poker.OrderInstances._

sealed trait ShowDown

object ShowDown {
  def apply(hands: List[Hand]): List[Hand] =
    hands.maximumList

}
