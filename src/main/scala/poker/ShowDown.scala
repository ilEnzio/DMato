package poker

import cats.data.NonEmptyList
import poker.OrderInstances._

sealed trait ShowDown

object ShowDown {
  def apply(hands: List[Hand]): List[Hand] = {

    val grouped = hands
      .map(_.cards)
      .map(Hand.rank(_))
      .groupBy(_.score)
      .toList
      .sortBy(_._1)
      .reverse
//
    val (_, bestHands) = grouped.head
    evaluateWinningHands(bestHands.head, bestHands.tail).toList
  }

  private def evaluateWinningHands(head: Hand, tail: List[Hand]): NonEmptyList[Hand] =
    tail.foldLeft(NonEmptyList(head, Nil)) { (s, v) =>
      if (hand_2Order.compare(v, s.head) == 0) v :: s
      else if (hand_2Order.compare(v, s.head) < 0) s
      else NonEmptyList(v, Nil)
    }

}
