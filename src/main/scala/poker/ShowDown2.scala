package poker

import cats.Order
import cats.data.NonEmptyList
import poker.OrderInstances.{hand_2Order, hand_2Ordering}

sealed trait ShowDown2

object ShowDown2 {
  def apply(hands: List[Hand_2]): List[Hand_2] = {

    val grouped = hands
      .map(_.cards)
      .map(Hand_2.rank(_))
      .groupBy(_.score)
      .toList
      .sortBy(_._1)
      .reverse
//
    val (_, bestHands) = grouped.head
    evaluateWinningHands(bestHands.head, bestHands.tail).toList

//      .sorted(bestHands)
//      .groupBy(hand_.score)
//      .toList
//      .sortBy(_._1)
//      .reverse

//
//    rankCategory match {
//      case StraightFlush => evaluateStraightFlush(bHands)
//      case FourOfAKind   => evaluateFourOfAKind(bHands)
//      case FullHouse     => evaluateFullHouse(bHands)
//      case Flush         => evaluateFlush(bHands)
//      case Straight      => evaluateStraight(bHands)
//      case ThreeOfAKind  => evaluateThreeOfKind(bHands)
//      case TwoPair       => evaluateTwoPair(bHands)
//      case Pair          => evaluatePairs(bHands)
//      case _             => evaluateHighCard(bHands)
//
//    }
  }

  private def evaluateWinningHands(head: Hand_2, tail: List[Hand_2]): NonEmptyList[Hand_2] =
    tail.foldLeft(NonEmptyList(head, Nil)) { (s, v) =>
      if (hand_2Order.compare(v, s.head) == 0) v :: s
      else if (hand_2Order.compare(v, s.head) < 0) s
      else NonEmptyList(v, Nil)
    }
//
//  private def evaluateHighCard(value: List[Hand]): List[Hand] = {
//    val ordered = value.sorted.reverse
//    evaluateWinningHands(ordered, handOrder)
//  }
//
//  private def evaluatePairs(value: List[Hand]): List[Hand] = {
//    val ordered = value.sorted(pairOrdering).reverse
//    evaluateWinningHands(ordered, pairOrder)
//  }
//
//  private def evaluateTwoPair(value: List[Hand]): List[Hand] = {
//    val ordered = value.sorted(twoPairOrdering).reverse
//    evaluateWinningHands(ordered, twoPairOrder)
//  }
//
//  private def evaluateThreeOfKind(value: List[Hand]): List[Hand] = {
//    val ordered = value.sorted(threeOfAKindOrdering).reverse
//    evaluateWinningHands(ordered, threeOfAKindOrder)
//  }
//
//  private def evaluateStraight(value: List[Hand]): List[Hand] = {
//    val ordered = value.sorted(straightOrdering).reverse
//    evaluateWinningHands(ordered, straightOrder)
//  }
//
//  private def evaluateFlush(value: List[Hand]): List[Hand] = {
//    val ordered = value.sorted(flushOrdering).reverse
//    evaluateWinningHands(ordered, flushOrder)
//  }
//
//  private def evaluateFullHouse(value: List[Hand]): List[Hand] = {
//    val ordered = value.sorted(fullHouseOrdering).reverse
//    evaluateWinningHands(ordered, fullHouseOrder)
//  }
//
//  private def evaluateFourOfAKind(value: List[Hand]): List[Hand] = {
//    val ordered = value.sorted(fourOfAKindOrdering).reverse
//    evaluateWinningHands(ordered, fourOfAKindOrder)
//  }
//
//  private def evaluateStraightFlush(value: List[Hand]): List[Hand] = {
//    val ordered = value.sorted(straightFlushOrdering).reverse
//    evaluateWinningHands(ordered, straightFlushOrder)
//  }
}
